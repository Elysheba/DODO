#========================================================================================@
#========================================================================================@
#' Benchmarking ambiguity DODO: load concept benchmarking cross references in DODO DB to 
#' test different BA cutoffs.
#'
#' Not exported to avoid unintended modifications of the DB.
#'
#' @param toImport the data frame of cross references with the following column
#' - DB1: *character* name of the database 1 (mandatory)
#' - id1: *character* short concept ID 1 (mandatory)
#' - DB2: *character* name of the database 2 (mandatory)
#' - id2: *character* short concept ID 2 (mandatory)
#' @param xrefDB a data frame with 2 columns ("DB1", "DB2") indicating couples
#' of DB considered close enough to define "xref". Identifiers from other
#' couples of DB will be considered as being "related"
#' @param concept either "Disease" or "Phenotype" or "Concept"
#'
benchmark_cross_references <- function(toImport, xrefDB, concept){
  ## Checks ----
  concept <- match.arg(concept, c("Concept", "Disease", "Phenotype"))
  tlc <- c(
    "DB1"="character",
    "id1"="character",
    "DB2"="character",
    "id2"="character"
  )
  mandatory <- c(
    "DB1",
    "id1",
    "DB2",
    "id2"
  )
  DODO:::check_df_to_import(toImport, tlc, mandatory)
  tlc <- c(
    "DB1"="character",
    "DB2"="character"
  )
  mandatory <- c(
    "DB1",
    "DB2"
  )
  DODO:::check_df_to_import(xrefDB, tlc, mandatory)
  
  ## Split cross references ----
  # xdb <- apply(xrefDB, 1, function(x) paste(sort(x), collapse=".."))
  xdb <- paste(
    pmin(xrefDB$DB1, xrefDB$DB2), pmax(xrefDB$DB1, xrefDB$DB2),
    sep=".."
  )
  ddb <- paste(
    pmin(toImport$DB1, toImport$DB2), pmax(toImport$DB1, toImport$DB2),
    sep=".."
  )
  sToImport <- list(
    'is_xref_bm'=toImport[which(ddb %in% xdb),],
    'is_related_bm'=toImport[which(!ddb %in% xdb),]
  )
  
  ## Import function ----
  import <- function(type, concept){
    toImport <- sToImport[[type]]
    
    ## Remove old benchmark edges
    DODO:::benchmark_remove_edges(type = type, concept = concept)
    
    ## Concepts ----
    cToImport1 <- toImport[, c("DB1", "id1")]
    cToImport2 <- toImport[, c("DB2", "id2")]
    colnames(cToImport1) <- colnames(cToImport2) <- c("database", "shortID")
    cToImport <- unique(rbind(cToImport1, cToImport2))
    DODO:::load_concept_names(cToImport, concept)
    
    ## References ----
    toImport$f <- paste(toImport$DB1, toImport$id1, sep=":")
    toImport$t <- paste(toImport$DB2, toImport$id2, sep=":")
    cql <- c(
      sprintf('MATCH (f:%s {name:row.f})', concept),
      sprintf('MATCH (t:%s {name:row.t})', concept),
      sprintf('MERGE (f)-[:%s]->(t)', type),
      sprintf('MERGE (f)<-[:%s]-(t)', type)
    )
    ## Import benchmark edges
    DODO:::import_in_dodo(neo2R::prepCql(cql), toImport[,c("f", "t")])
    
    ## Update ambiguity ----
    DODO:::call_dodo(neo2R::cypher, query=neo2R::prepCql(c(
      sprintf('MATCH (c)-[f:%s]->(r)-[b:%s]->(c)', type, type),
      'MATCH (r)-[:is_in]->(d:Database)',
      'WITH c.name AS cname, d.name AS refDB,',
      'count(r) AS rcount,',
      'collect(f) AS allf, collect(b) AS allb',
      'FOREACH(e in allf | set e.FA=rcount)',
      'FOREACH(e in allb | set e.BA=rcount)'
    )))
    DODO:::call_dodo(
      neo2R::cypher,
      query=neo2R::prepCql(c(
        sprintf(
          'MATCH (f:%s)-[r1:%s {BA:1}]->(t:%s)',
          concept, type, concept
        ),
        sprintf('MERGE (f)-[r2:%s]->(t)', paste0(type, "_nba")),
        'ON CREATE SET r2.BA = r1.BA ON CREATE SET r2.FA = r1.BA'
      ))
    )
    ## when updating new resources, the ambiguity will be calculated on the fly
    ## the old _nba edge might be deprecated and needs to be deleted
    DODO:::call_dodo(
      neo2R::cypher,
      query=neo2R::prepCql(c(
        sprintf(
          'MATCH (f:%s)-[r:%s]->(t:%s) WHERE r.BA > 1',
          concept, type, concept
        ),
        sprintf('MATCH (f)-[nba:%s]->(t)', paste0(type, "_nba")),
        'DELETE nba'
      ))
    )
  }
  
  ## xref ----
  import(type = "is_xref_bm",concept = concept)
  
  ## related ----
  import(type = "is_related_bm", concept = concept)
}

#========================================================================================@
#========================================================================================@
#' Removing benchmark edges
#'
#' Not exported to avoid unintended modifications of the DB.
#' 
#' @param concept either "Disease" or "Phenotype"
#' @param type edge type
benchmark_remove_edges <- function(type, concept){
  ## Remove old benchmarking edges before adding new ones
  call_dodo(
    neo2R::cypher,
    query=neo2R::prepCql(c(
      sprintf(
        paste0('MATCH (f:%s)-[r:', paste(type, paste0(type, "_nba"), sep = "|"),']->(t:%s)'),
        concept, concept
      ),
      'DELETE r'
    ))
  )
}

#========================================================================================@
#========================================================================================@
#' Benchmarking: Convert concept identifiers using benchmark edges
#' 
#' Not exported to avoid unintended modifications of the DB.
#' 
#' To assess the different cutoffs used to define *is_xref* and *is_related* edges
#' the convert_concept function is used. As additional edges are defined (_bm) for testing
#' this function uses these edges to compare conversion using different BA cutoffs
#' 
#' @param from a vector with identifier to convert formatted as DB:id (eg. "MONDO:0005027)
#' @param to database to convert to (default = NULL, no filtering)
#' @param from.concept concept (disease or phenotype) of from
#' @param to.concept concept (disease or phenotype) to convert to
#' @param deprecated include deprecated identifiers (default: false)
#' @param verbose show query input (default: FALSE)
#' 
#' @return a dataframe with three columns:
#' - from: identifier to convert
#' - to: returned conversion
#' 
#' 
benchmark_convert_concept <- function(from, 
                            to = NULL,
                            from.concept = c("Disease", "Phenotype"),
                            to.concept = c("Disease", "Phenotype"),
                            deprecated = FALSE,
                            # transitive_ambiguity = 1,
                            # intransitive_ambiguity = NULL,
                            verbose = FALSE){
  from.concept <- match.arg(from.concept, c("Disease", "Phenotype"))
  to.concept <- match.arg(to.concept, c("Disease", "Phenotype"))
  db.to <- match.arg(to, c(NULL, list_database()$database))
  from <- setdiff(from, NA)
  stopifnot(is.character(from), length(from)>0)
  
  # stopifnot(is.null(transitive_ambiguity) || transitive_ambiguity == 1,
  #           is.null(intransitive_ambiguity) || intransitive_ambiguity == 1)
  
  # if(is.null(transitive_ambiguity)){
  #   transitivity <- "is_xref_bm>"
  # }else{
    transitivity <- "is_xref_bm_nba"
  # }
  
  # if(is.null(intransitive_ambiguity)){
  #   relationship <- ":is_related_bm|is_xref_bm*0..1"
  # }else{
    relationship <- ":is_related_bm_nba|is_xref_bm_nba*0..1"
  # }
  
  #############################################@
  ## To convert identifiers between concepts, first they are converted within the concept (d-d, p-p) = B1 
  ## followed by a conversion between concepts (d-p, p-d) = B2
  
  ## Convert within concepts: B1
  cql <- c(
    sprintf('MATCH (f:%s) WHERE f.name IN $from',
            from.concept),
    'CALL apoc.path.expandConfig(',
    sprintf('f, {uniqueness:"NODE_GLOBAL", relationshipFilter:"%s>"}',
            transitivity),
    ') YIELD path',
    'WITH (nodes(path))[0] as s, last(nodes(path)) as e',
    sprintf('MATCH (e)-[%s]->(e2)',
            relationship),
    'RETURN DISTINCT',
    's.name as from, e2.name as to'
    # '(nodes(path))[0].name AS from, ',
    # 'last(nodes(path)).name AS to, ',
    # 'size(relationships(path)) AS hops'
  )
  b1 <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    parameters = list(from = as.list(from)),
    result = "row")
  
  if(nrow(b1) == 0){
    b1 <- tibble::tibble(from = character(),
                         to = character())
  }
  
  #############################################@
  ## Convert between concepts: B2
  if(from.concept != to.concept){
    cql <- c(
      sprintf('MATCH (f:%s)-[:has_pheno]-(t:%s)',
              from.concept,
              to.concept),
      'WHERE f.name IN $from',
      "RETURN DISTINCT f.name as from, t.name as to"
    )
    b2 <- call_dodo(
      neo2R::cypher,
      neo2R::prepCql(cql),
      parameters = list(from = as.list(from)),
      result = "row"
    )
  }else{
    b2 <- tibble::tibble(from = character(),
                         to = character())
  }
  
  toRet <- dplyr::bind_rows(b1,
                            b2) %>%
    tibble::as_tibble() %>%
    dplyr::distinct() %>%
    dplyr::mutate(deprecated = FALSE)
  
  #############################################@
  ## Return deprecated identifiers
  
  if(deprecated){
    cql <- c(
      'MATCH (f:Concept)<-[:is_alt]-(t:Concept)',
      'WHERE f.name IN $from',
      "RETURN DISTINCT f.name as from, t.name as to"
    )
    b3 <- call_dodo(
      neo2R::cypher,
      neo2R::prepCql(cql),
      parameters = list(from = as.list(from)),
      result = "row"
    ) %>%
      tibble::as_tibble()
  }
  if(!deprecated || nrow(b3) == 0){
    b3 <- tibble::tibble(from = character(),
                         to = character(),
                         deprecated = logical())
  }else{
    b3 <- b3 %>%
      dplyr::distinct() %>%
      dplyr::mutate(deprecated = TRUE)
  }
  
  toRet <- dplyr::bind_rows(toRet,
                            b3)
  
  if(!is.null(to)){
    toRet <- toRet %>% 
      dplyr::filter(grepl(glue::glue("{db.to}"), to))
  }
  
  return(toRet)
}

#========================================================================================@
#========================================================================================@
#' Ambiguity calculations
#' 
#' Calculating the forward and backward ambiguity on cross-reference edges
#' 
#' @param DODO_crossId object with cross-reference information
#' 
#' Not exported to avoid unintended modifications of the DB.
#' @return object with cross-reference information and added ambiguity edges
#' 
calculate_ambiguity <- function(DODO_crossId){
  ## Count REF per ID and per DB ----
  dbByRef <- DODO_crossId %>%
    dplyr::select(node = dbid1, refdb = dbid2) %>%
    dplyr::bind_rows(DODO_crossId %>% dplyr::select(node = dbid2, refdb = dbid1)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(refdb = stringr::str_remove(refdb, ":.*")) %>%
    dplyr::add_count(node, refdb) %>%
    dplyr::distinct() %>%
    dplyr::mutate(comb = paste(node, refdb, sep = "_"))
  tmp <- dplyr::bind_rows(
    DODO_crossId %>% dplyr::select(dbid1 = dbid1,
                                   DB1 = DB1,
                                   id1 = id1, 
                                   dbid2 = dbid2,
                                   id2 = id2, 
                                   DB2 = DB2),
    DODO_crossId %>% dplyr::select(dbid1 = dbid2,
                                   id1 = id2, 
                                   DB1 = DB2,
                                   dbid2 = dbid1,
                                   id2 = id1,
                                   DB2 = DB1)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(comb1 = paste(dbid1, DB2, sep = "_"),
                  comb2 = paste(dbid2, DB1, sep = "_")) %>%
    dplyr::mutate(FA = dbByRef$n[match(comb1, dbByRef$comb)],
                  BA = dbByRef$n[match(comb2, dbByRef$comb)])
  DODO_crossId <- tmp %>%
    dplyr::select(DB1, id1, DB2, id2, dbid1, dbid2, FA, BA)
  return(DODO_crossId)
}
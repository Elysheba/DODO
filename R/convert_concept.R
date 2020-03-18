#' Convert concept identifiers
#' 
#' Another core functionality is the ability to convert concept identifiers between different 
#' databases and types (disease or phenotype). By default a backward ambiguity of one is taken,
#' with no limitation on forward ambiguity. 
#' 
#' @param from a vector with identifier to convert formatted as DB:id (eg. "MONDO:0005027)
#' @param to database to convert to (default = NULL, no filtering)
#' @param from.concept concept (disease or phenotype) of from
#' @param to.concept concept (disease or phenotype) to convert to
#' @param deprecated include deprecated identifiers (default: false)
#' @param transitive_ambiguity backward ambiguity while using transitivity to identify cross-references (default: 1)
#' @param intransitive_ambiguity specification for backward ambiguity used in the final step of conversion (default: no filter)
#' @param verbose show query input (default: FALSE)
#' 
#' @return a dataframe with three columns:
#' - from: identifier to convert
#' - to: returned conversion
#' 
#' @export
#' 
convert_concept <- function(from, 
                            to = NULL,
                            from.concept = c("Disease", "Phenotype"),
                            to.concept = c("Disease", "Phenotype"),
                            deprecated = FALSE,
                            transitive_ambiguity = 1,
                            intransitive_ambiguity = NULL,
                            verbose = FALSE){
   from.concept <- match.arg(from.concept, c("Disease", "Phenotype"), several.ok = FALSE)
   to.concept <- match.arg(to.concept, c("Disease", "Phenotype"), several.ok = FALSE)
   db.to <- match.arg(to, c(NULL, list_database()$database))
   from <- setdiff(from, NA)
   stopifnot(is.character(from), length(from)>0)
   
   stopifnot(is.null(transitive_ambiguity) || transitive_ambiguity == 1,
             is.null(intransitive_ambiguity) || intransitive_ambiguity == 1)
   
   if(is.null(transitive_ambiguity)){
      transitivity <- "is_xref>"
   }else{
      transitivity <- "is_xref_nba"
   }
   
   if(is.null(intransitive_ambiguity)){
      relationship <- ":is_related|is_xref*0..1"
   }else{
      relationship <- ":is_related_nba|is_xref_nba*0..1"
   }
   
   if(!deprecated){
      #############################################@
      ## To convert identifiers between concepts, first they are converted within the concept (d-d, p-p) = B1 
      ## followed by a conversion between concepts (d-p, p-d) = B2
      
      # cql <- c(
      #    sprintf('MATCH (f:%s)-[%s]-(t:%s)',
      #            from.concept,
      #            relationship,
      #            to.concept),
      #    'WHERE f.name IN $from',
      #    "RETURN DISTINCT f.name as from, t.name as to"
      # )
      # r1 <- call_dodo(
      #    neo2R::cypher,
      #    prepCql(cql),
      #    parameters = list(from = as.list(from)),
      #    result = "row")
      
      ## Convert within concepts: B1
      cql <- c(
         sprintf('MATCH (f:%s)-[%s]-(t:%s) WHERE f.name IN $from',
            from.concept,
            relationship,
            to.concept),
         'CALL apoc.path.expandConfig(',
         sprintf('f, {uniqueness:"NODE_GLOBAL", relationshipFilter:"%s>"}',
                 transitivity),
         ') YIELD path',
         'WITH f as f, (nodes(path))[0] as s, last(nodes(path)) as e',
         sprintf('MATCH (e)-[%s]->(e2)',
                 relationship),
         'RETURN DISTINCT',
         's.name as from, e2.name as to'
         # '(nodes(path))[0].name AS from, ',
         # 'last(nodes(path)).name AS to, ',
         # 'size(relationships(path)) AS hops'
      )
      toRet <- call_dodo(
            neo2R::cypher,
            prepCql(cql),
            parameters = list(from = as.list(from)),
            result = "row") %>%
         tibble::as_tibble()
      
      if(nrow(toRet) == 0){
         b1 <- tibble::tibble(from = character(),
                              to = character())
      }else{
         b1 <- toRet
      }
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
         prepCql(cql),
         parameters = list(from = as.list(from)),
         result = "row"
      )
   }
   if(from.concept == to.concept || nrow(b2) == 0){
      b2 <- tibble::tibble(from = character(),
                           to = character())
   }else{
      b2 <- tibble::as_tibble(b2)
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
         prepCql(cql),
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

####################################@
#' Get related concepts
#' 
#' The specific conversion procedure is recommended for the ontologies that 
#' consider disease concepts that are less connected through *is_xref* edges
#' (e.g. ICD10, ICD9, ClinVar) and are mostly only related to other identifiers 
#' using *is_related* edges. This limits or removes the effect of the transitive 
#' mapping and not return all cross-references. Therefore, for these ontologies it 
#' is recommend that in addition to the standard conversion, an additional step 
#' of intransitive mapping is performed which is implemented in the function *get_related*.  
#' 
#' @param from a vector with identifier to convert formatted as DB:id (eg. "MONDO:0005027)
#' @param to database to convert to (default = NULL, no filtering)
#' @param from.concept concept (disease or phenotype) of from
#' @param to.concept concept (disease or phenotype) to convert to
#' @param deprecated include deprecated identifiers (default: false)
#' @param transitive_ambiguity backward ambiguity while using transitivity to identify cross-references (default: 1)
#' @param intransitive_ambiguity specification for backward ambiguity used in the final step of conversion (default: no filter)
#' @param verbose show query input (default: FALSE)
#' #' 
#' @return a dataframe with three columns:
#' - from: identifier to convert
#' - to: returned conversion
#' 
#' @export
#' 
get_related <- function(from, 
                        to = NULL,
                        from.concept = c("Disease", "Phenotype"),
                        to.concept = c("Disease", "Phenotype"),
                        deprecated = FALSE,
                        transitive_ambiguity = 1,
                        intransitive_ambiguity = NULL,
                        verbose = FALSE){
   dodoConv <- convert_concept(from = from,
                               to = to,
                               from.concept = from.concept,
                               to.concept = to.concept,
                               deprecated = deprecated,
                               transitive_ambiguity = transitive_ambiguity,
                               intransitive_ambiguity = intransitive_ambiguity,
                               verbose = verbose)
   ## Additional step
   cql <- c("MATCH (n:Disease)-[r:is_xref*0..1]-(n1:Disease)",
            "WHERE n.name in $from",
            "RETURN n.name as from2, n1.name as to2")
   relConv <- call_dodo(cypher, 
                        prepCql(cql),
                        parameters = list(from = as.list(unique(dodoConv$to))),
                        result = "row")
   finConv <- dodoConv %>%
      left_join(relConv,
                by = c("to" = "from2")) %>%
      select(from = from,
             to = to2) %>%
      distinct()
   return(finConv)
}



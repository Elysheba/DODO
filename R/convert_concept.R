#' Convert identifiers
#'
#' Convert identifiers between different databases and types (disease or phenotype).
#'
#' @param ids String. A vector with identifier to convert formatted as DB:id (eg. "MONDO:0005027)
#' @param to_db String. A database to convert to (default = NULL, no filtering)
#' @param relationship String. Type of relationship to extract ("xref", "parent", "child",
#' "phenotype", "disease", "alternative"). Default: xref.
#' @param direct Boolean. Only getting direct relationships or moving through the disease
#' network to return all connected relationship. (default =)
#' @param sep String. Separator used, default: ":".
#' @param verbose show query input (default = FALSE)
#'
#' @details
#' The function identifies cross-references, parent-child hierarchies, disease-phenotype
#' or alternative relationships.
#' Direct = T will return only the direct relations, direct = F will move through
#' the nodes to retrieve connected relationships.
#'
#' @return a data.table with three columns:
#' - from: identifier to convert
#' - to: returned conversion
#' - relation: type of relation between nodes
#'
#' @examples
#' \dontrun{
#' convert_concept(
#'   id = "MONDO:0005027",
#'   to_db = "EFO",
#'   relationship = "xref",
#'   direct = T
#' )
#' }
#' @export
#' @import data.table
#' @import magrittr
convert_concept <- function(ids,
                            to_db = NULL,
                            relationship = c("xref"),
                            direct = F,
                            sep = ":",
                            verbose = FALSE) {
  checkmate::assert(checkmate::checkNull(to_db),
    checkmate::checkNames(to_db,
      subset.of = c(list_database()$database)
    ),
    combine = "or"
  )
  checkmate::assertChoice(relationship, choices = c(
    "xref", "parent", "child",
    "phenotype", "disease",
    "alternative"
  ))
  
  checkmate::assertLogical(direct)

  ids <- na.omit(ids)
  ids <- DODO:::check_divider(ids, sep = sep, format = "in")

  if ("is_xref" %in% relationship & direct) relationship <- gsub("is_xref", "is_xref_many", relationship)
  
  edge_types <- list(
    "xref" = c("-[r:is_xref|is_xref_many]->", "-[r:is_xref]->", "is_xref>", "-[:is_xref|is_xref_many]->"),
    "parent" = c("-[r:is_a]->", "-[r:is_a]->", "is_a>", "-[:is_a]->"),
    "child" = c("<-[r:is_a]-", "<-[r:is_a]-", "<is_a", "<-[:is_a]-"),
    "phenotype" = "-[r:has_phenotype]->",
    "disease" = "<-[r:has_phenotype]-",
    "alternative" = "-[r:has_alternative_id]-"
  )

  if (any(c("xref", "parent", "child") %in% relationship) & !direct) {
    cql <- DODO:::indirect_cql(
      edge_types = edge_types,
      relationship = relationship,
      to_db = to_db,
      network = F
    )
  } else {
    cql <- DODO:::direct_cql(
      edge_types = edge_types,
      relationship = relationship,
      to_db = to_db
    )
  }

  # ICD ontology and Cortellis is a bit difficult.. they don't match
  # directly with any other ontologies
  # so.. let's first get the direct cross-references, then the normal conversion after that
  if(any(grepl("ICD|Cortellis", ids)) & "xref" %in% relationship){
    toRet <- DODO:::is_many_xrefs(ids = ids, 
                                  edge_types = edge_types,
                                  to_db = to_db, 
                                  relationship = relationship, 
                                  network = F,
                                  verbose = verbose)
  }else{
    toRet <- DODO:::process_cql(cql,ids = ids, to_db = to_db, 
                              relationship = relationship, verbose = verbose)
  }

  if (ncol(toRet) != 3) {
    toRet <- data.table::data.table(
      from = ids,
      to = NA,
      relationship = relationship
    )
  }
  if (!all(ids %in% toRet$from)) {
    toRet <- rbind(
      toRet,
      data.table::data.table(
        from = setdiff(ids, toRet$from),
        to = NA,
        relationship = relationship
      )
    )
  }
  toRet <- toRet[, `:=`(
    from = DODO:::check_divider(from, sep = sep, format = "out"),
    to = DODO:::check_divider(to, sep = sep, format = "out")
  )]
  return(toRet)
}


#' Convert identifiers
#'
#' Convert identifiers between different databases and types (disease or phenotype).
#' and return the full relationships to be explored as a graph or network (e.g. igraph)
#'
#' @param ids String. A vector with identifier to convert formatted as DB:id (eg. "MONDO:0005027)
#' @param to_db String. A database to convert to (default = NULL, no filtering)
#' @param relationship String. Type of relationship to extract ("xref", "parent", "child",
#' "phenotype", "disease", "alternative"). Default: xref.
#' @param direct Boolean. Only getting direct relationships or moving through the disease
#' network to return all connected relationship.
#' @param sep String. Separator used, default: ":".
#' @param verbose show query input (default = FALSE)
#'
#' @details
#' The function identifies cross-references, parent-child hierarchies, disease-phenotype
#' or alternative relationships.
#' Direct = T will return only the direct relations, direct = F will move through
#' the nodes to retrieve connected relationships.
#'
#' @return a data.table with three columns:
#' - from: identifier to convert
#' - to: returned conversion
#' - relation: type of relation between nodes
#'
#' @examples
#' \dontrun{
#' get_network(
#'   id = "MONDO:0005027",
#'   to_db = "EFO",
#'   relationship = "xref",
#'   direct = T
#' )
#' }
#' @export
#' @import data.table
#' @import magrittr
get_network <- function(ids,
                        to_db = NULL,
                        relationship = c("xref"),
                        direct = F,
                        sep = ":",
                        verbose = FALSE) {
  checkmate::assert(checkmate::checkNull(to_db),
    checkmate::checkNames(to_db,
      subset.of = c(list_database()$database)
    ),
    combine = "or"
  )
  checkmate::assertChoice(relationship, choices = c(
    "xref", "parent", "child",
    "phenotype", "disease",
    "alternative"
  ))
  checkmate::assertLogical(direct)

  ids <- na.omit(ids)
  ids <- DODO:::check_divider(ids, sep = sep, format = "in")

  if ("is_xref" %in% relationship & direct) relationship <- gsub("is_xref", "is_xref_many", relationship)
  edge_types <- list(
    "xref" = c("-[r:is_xref|is_xref_many]->", "-[r:is_xref]->", "is_xref>", "-[:is_xref|is_xref_many]->"),
    "parent" = c("-[r:is_a]->", "-[r:is_a]->", "is_a>", "-[:is_a]->"),
    "child" = c("<-[r:is_a]-", "<-[r:is_a]-", "<is_a", "<-[:is_a]-"),
    "phenotype" = "-[r:has_phenotype]->",
    "disease" = "<-[r:has_phenotype]-",
    "alternative" = "-[r:has_alternative_id]-"
  )

  if (any(c("xref", "parent", "child") %in% relationship) & !direct) {
    cql <- DODO:::indirect_cql(
      edge_types = edge_types,
      relationship = relationship,
      to_db = to_db,
      network = T
    )
  } else {
    cql <- DODO:::direct_cql(
      edge_types = edge_types,
      relationship = relationship,
      to_db = to_db
    )
  }
  
  # ICD ontology and Cortellis is a bit difficult.. they don't match
  # directly with any other ontologies
  # so.. let's first get the direct cross-references, then the normal conversion after that
  if(any(grepl("ICD|Cortellis", ids)) & "xref" %in% relationship){
    toRet <- DODO:::is_many_xrefs(ids = ids, 
                                  edge_types = edge_types,
                                  to_db = to_db, 
                                  relationship = relationship, 
                                  network = T,
                                  verbose = verbose)
  }else{
    toRet <- DODO:::process_cql(cql,ids = ids, to_db = to_db, 
                                relationship = relationship, verbose = verbose)
  }
  
  if (ncol(toRet) != 3) {
    toRet <- NULL
  }
  toRet <- toRet[, `:=`(
    from = DODO:::check_divider(from, sep = sep, format = "out"),
    to = DODO:::check_divider(to, sep = sep, format = "out")
  )]
  return(toRet)
}


#' helper function for indirect cql query
#'
#' will return the cql query to use the apoc.path.expandConfig
#' based on a data.table or network to be returned
#'
#' @param edge_types List. Edge types to query. See [DODO::convert_concept]
#' @param to_db String. Database to filter on.
#' @param relationship String. Relationship to filter for.
#' @param network Boolean. Network or data.table to be returned as output.
#'
indirect_cql <- function(edge_types = edge_types,
                         to_db = to_db,
                         relationship = relationship,
                         network = F) {
  checkmate::assertLogical(network)
  
  cql <- c(
    sprintf("MATCH (n1)%s(n2) WHERE n1.dbid IN $from", edge_types[[relationship]][2]),
    "CALL apoc.path.expandConfig(",
    sprintf('n1, {uniqueness:"NODE_GLOBAL", relationshipFilter:"%s"}', edge_types[[relationship]][3]),
    ") YIELD path",
    "WITH n1 as n1, (nodes(path))[0] as s, last(nodes(path)) as n2",
    sprintf("MATCH (n2)%s(n3)%s", 
            edge_types[[relationship]][4],
            ifelse(is.null(to_db), "", "-[:is_in]->(db) WHERE db.short_name in $to_db")),
    "RETURN DISTINCT",
    sprintf("%s.dbid as from, n3.dbid as to", ifelse(network, "n2", "n1"))
  )
  return(cql)
}

#' helper function for direct cql query
#'
#' will return the cql query to get the direct relations
#'
#' @param edge_types List. Edge types to query. See [DODO::convert_concept]
#' @param to_db String. Database to filter on.
#' @param relationship String. Relationship to filter for.
#'
direct_cql <- function(edge_types = edge_types,
                       to_db = to_db,
                       relationship = relationship) {
  cql <- c(
    sprintf(
      "MATCH (n1)%s(n2)",
      edge_types[[relationship]][1]
    ),
    "WHERE n1.dbid IN $from",
    ifelse(is.null(to_db), "", "AND n2.DB in $to_db"),
    "RETURN DISTINCT n1.dbid as from, n2.dbid as to"
  )
  return(cql)
}

#' helper function to send the query and format the output
#'
#' Prepares, send and return the output of the query
#'
#' @param cql String. Cql query to send
#' @param ids String. Vector of identifiers
#' @param to_db String. database to filter for.
#' @param verbose Boolean. verbose output.
#'
#' @import data.table
#' @import magrittr
process_cql <- function(cql = cql, ids = ids, relationship = relationship, 
                        to_db = to_db, verbose = verbose) {
  if (verbose) {
    cat(cql, sep = "\n")
  }
  toRet <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    parameters = list(
      from = as.list(ids),
      to_db = as.list(to_db)
    ),
    result = "row"
  ) %>%
    data.table::as.data.table() %>%
    .[, relationship := relationship]
  return(toRet)
}

#' helper function for ICD/Cortellis identifiers
#' Prepares the query if you need to convert with cortellis/ICD
#' 
#' @param cql String. Cql query to send
#' @param ids String. Vector of identifiers
#' @param to_db String. database to filter for.
#' @param verbose Boolean. verbose output.
#'
#' @import data.table
#' @import magrittr
is_many_xrefs <- function(ids = ids, 
                          edge_types = edge_types,
                          relationship = relationship, 
                          to_db = to_db, 
                          network = network, 
                          verbose = verbose){
  cql0 <- DODO:::direct_cql(
    edge_types = edge_types,
    relationship = relationship,
    to_db = NULL
  )
  to_many <- DODO:::process_cql(cql0,
                                ids = ids, 
                                to_db = NULL, 
                                relationship = relationship, 
                                verbose = verbose) %>% 
    setnames(., "to", "intermediate_id", skip_absent=TRUE) 
  
  cql <- DODO:::indirect_cql(
      edge_types = edge_types,
      relationship = relationship,
      to_db = to_db,
      network = network
    )
  to_xref <- DODO:::process_cql(cql,
                            ids = to_many$intermediate_id, 
                            to_db = to_db, 
                            relationship = relationship, 
                            verbose = verbose) %>% 
    setnames(., "from", "intermediate_id", skip_absent=TRUE)  
  toRet <- merge(to_many, to_xref, by = "intermediate_id") %>% 
    .[, .(from, to)] %>% 
    .[, relationship := "xref"] %>% 
    unique()
  
  if(network){
    toRet1 <- rbind(
      toRet, 
      to_xref[!intermediate_id %in% toRet$from] %>% 
        `colnames<-`(c("from", "to", "relationship"))
    )
  }
  return(toRet)
}
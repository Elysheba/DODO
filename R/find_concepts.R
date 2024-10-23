###############################################################################@
#' Find concept by identifier 
#' 
#' @param ids a character vector of concept to search formatted as "DB:id" (e.g. "MONDO:0005027")
#' 
#' @return A character vector of concept identfiers in DODO
#' 
#' @examples 
#' toRet <- find_id(ids = c("MONDO:0005027","MONDO:0100033"))
#' 
#' @export
#' 
find_id <- function(ids){
  cql <- c('MATCH (n)',
           'WHERE n.dbid IN $from',
           'RETURN n.dbid'
  )
  
  toRet <- call_dodo(neo2R::cypher,
                     neo2R::prepCql(cql),
                     parameters = list(from = as.list(ids)),
                     result = "row")
  
  return(unname(unlist(toRet)))  
}

###############################################################################@
#' Find concept by term 
#' 
#' Querying for concept through search terms (label, synonym)
#' 
#' @param term a character vector of terms to search (e.g. "epilep")
#' @param fields the field(s) where to look for matches (label, synonym).
#'  
#' @return A character vector of concept identifiers in DODO
#' 
#' 
#' @export

find_term <- function(term,
                      fields = c("label", "synonym", "definition")){
  checkmate::assertNames(x = fields, subset.of = c("label", "synonym", "definition"))
 
  st <- tolower(term)
  
  query <- NULL
  if("label" %in% fields){
    query <- c(query,
               sprintf('n.name CONTAINS "%s"', st)
    )
  }
  if("synonym" %in% fields){
    query <- c(query,
               sprintf('n.synonyms CONTAINS "%s"',
                       st)
    )
  }
  if("definition" %in% fields){
    query <- c(query,
               sprintf('n.def CONTAINS "%s"',
                       st)
               )
  }
  query <- paste(query, collapse = " OR ")
  
  cql <- c(sprintf('MATCH (n) WHERE %s' ,
                   query),
           'RETURN DISTINCT n.dbid')
  toRet <- call_dodo(neo2R::cypher,
                     neo2R::prepCql(cql),
                     parameters = NULL,
                     result = "row")
  return(unname(unique(unlist(toRet))))
}
#========================================================================================@
#========================================================================================@
#' Return entire disease ontology
#' 
#' Return all nodes for a particular disease ontology
#' 
#' @param database disease ontology to return.
#' 
#' @seealso \code{\link{list_database}}
#' 
#' @export
#' 
get_ontology <- function(database){
  checkmate::assertNames(database, subset.of = list_database()$database)
  
  ## Get concepts ----
  cql <- c(
    'MATCH (n)-[:is_in]->(:Database {db:$database})',
    'RETURN DISTINCT n.dbid'
  )
  
  ont <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    parameters=list(database=database),
    result="row"
  )
  
  cql <- c(
    'MATCH (n)-[r:is_a]-(n1) WHERE n.dbid IN $from AND n1.dbid IN $from',
    'RETURN DISTINCT n.dbid as child, type(r) as type, n1.dbid as parent'
  )
  
  toRet <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    parameters=list(from = as.list(ont$n.dbid)),
    result="row"
  ) %>% 
    as.data.table()
  
  ## Return all results ----
  return(toRet)
}

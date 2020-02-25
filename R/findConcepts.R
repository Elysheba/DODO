###############################################################################@
#' Find concept by identifier 
#' 
#' @param ids a character vector of concept to search formatted as "DB:id" (e.g. "MONDO:0005027")
#' 
#' @return A character vector of concept identfiers in DODO
#' 
#' @examples 
#' findID(id = c("MONDO:0005027","MONDO:0100033"))
#' 
#' @export
#' 
findID <- function(ids){
  cql <- c('MATCH (n)',
           'WHERE n.name IN $from',
           'RETURN n.name'
  )
  
  toRet <- call_dodo(neo2R::cypher,
                     prepCql(cql),
                     parameters = list(from = as.list(ids)),
                     result = "row")
  
  return(unname(unlist(toRet)))  
}

###############################################################################@
#' Find concept by term 
#' 
#' Querying for concept through searchterms (label, synonym, definition)
#' 
#' @param term a character vector of terms to search (e.g. "epilep")
#' @param fields the field(s) where to look for matches (label, synonym, definition).
#'  
#' @return A character vector of concept identfiers in DODO
#' 
#' @export

findTerm <- function(term,
                     fields = c("label", "synonym", "definition")){
  fields <- match.arg(fields, c("label", "synonym", "definition"), several.ok = TRUE)
 
  st <- toupper(term)
  
  query <- NULL
  if("label" %in% fields){
    query <- c(query,
               sprintf('s.value_up CONTAINS "%s"', st)
    )
  }
  if("synonym" %in% fields){
    query <- c(query,
               sprintf('n.label_up CONTAINS "%s"',
                       st)
    )
  }
  if("definition" %in% fields){
    query <- c(query,
               sprintf('n.definition_up CONTAINS "%s"',
                       st)
               )
  }
  query <- paste(query, collapse = " OR ")


  
  cql <- c(sprintf('MATCH (s:Synonym)-[:is_known_as]-(n) WHERE',
                   st),
           query,
           'RETURN DISTINCT n.name')
  toRet <- call_dodo(neo2R::cypher,
                     prepCql(cql),
                     parameters = NULL,
                     result = "row")
  return(unname(unlist(toRet)))
}
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
           'WHERE n.name IN $from',
           'RETURN n.name'
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
#' Querying for concept through searchterms (label, synonym)
#' 
#' @param term a character vector of terms to search (e.g. "epilep")
#' @param fields the field(s) where to look for matches (label, synonym).
#'  
#' @return A character vector of concept identfiers in DODO
#' 
#' 
#' @export

find_term <- function(term,
                     fields = c("label", "synonym")){
  fields <- match.arg(fields, c("label", "synonym"), several.ok = TRUE)
 
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
  # if("definition" %in% fields){
  #   query <- c(query,
  #              sprintf('n.definition_up CONTAINS "%s"',
  #                      st)
  #              )
  # }
  query <- paste(query, collapse = " OR ")


  
  cql <- c(sprintf('MATCH (s:Synonym)-[:is_known_as]-(n) WHERE %s' ,
                   query),
           'RETURN DISTINCT n.name')
  toRet <- call_dodo(neo2R::cypher,
                     neo2R::prepCql(cql),
                     parameters = NULL,
                     result = "row")
  return(unname(unlist(toRet)))
}
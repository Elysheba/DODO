#' Embryonic function to get full ontology
#' 
#' @export
#' 
get_ontology <- function(database){
   match.arg(database, list_db()$database, several.ok = FALSE)
   
   ## Get concepts ----
   cql <- c(
      'MATCH (n:Concept)-[:is_in]->(:Database {name:$database})',
      'RETURN DISTINCT n.name AS id, n.label AS label, n.definition AS definition, ',
                   'n.shortID AS shortID, n.level AS level, labels(n) AS type')
   
   concepts <- call_dodo(
      neo2R::cypher,
      prepCql(cql),
      parameters=list(database=database),
      result="row"
   )
   toRet <- concept
   
   ## Build disNet
   # if(database == "EFO"){
   #    avoid = NULL
   # }else{
   #    avoid = "EFO"
   # }
   
   ## Get parents ----
   # toRet <- build_disNet(ids = concepts$id, avoidOrigin = avoid)
   ## Return all results ----
   return(toRet)
}

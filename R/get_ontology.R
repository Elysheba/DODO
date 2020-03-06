#' Embryonic function to get full ontology
#' 
#' @export
#' 
get_ontology <- function(database){
   match.arg(database, list_db()$database, several.ok = FALSE)
   
   ## Get concepts ----
   cql <- c(
      'MATCH (n:Concept)-[:is_in]->(:Database {name:$database})',
      'RETURN DISTINCT n.name'
      )
   
   ont <- call_dodo(
      neo2R::cypher,
      prepCql(cql),
      parameters=list(database=database),
      result="row"
   )
   
   ## Build disNet
   if(database == "EFO"){
      avoid = NULL
   }else{
      avoid = "EFO"
   }

   ## Get parents ----
   toRet <- build_disNet(id = ont$n.name, avoidOrigin = avoid)
   ## Return all results ----
   return(toRet)
}

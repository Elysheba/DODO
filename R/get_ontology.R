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
   match.arg(database, list_database()$database, several.ok = FALSE)
   
   ## Get concepts ----
   cql <- c(
      'MATCH (n:Concept)-[:is_in]->(:Database {name:$database})',
      'RETURN DISTINCT n.name'
      )
   
   ont <- call_dodo(
      neo2R::cypher,
      neo2R::prepCql(cql),
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
   
   ## Update levels if necessary 
   cql <- c("MATCH (n:Concept)-[r:is_in]->(d:Database)",
            "WHERE n.name in $from AND d.name in $database",
            "RETURN n.name as id, r.level as level")
   l <- call_dodo(
      neo2R::cypher,
      neo2R::prepCql(cql),
      result = "row",
      parameters = list(from = as.list(toRet$nodes$id),
                        database = as.list(database)))
   toRet$nodes <- toRet$nodes %>%
      dplyr::mutate(level = l$level[match(id, l$id)])

   ## Return all results ----
   return(toRet)
}

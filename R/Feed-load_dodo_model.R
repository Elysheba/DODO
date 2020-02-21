#' Feeding DODO: Load DODO data model in neo4j
#'
#' Not exported to avoid unintended modifications of the DB.
#'
load_dodo_model <- function(){
   pkgname <- utils::packageName()
   ## Model
   cqlFile <- system.file(
      "documentation", "data-model", "DODO.cql",
      package=pkgname
   )
   queries <- neo2R::readCql(cqlFile)
   for(query in queries){
      call_dodo(neo2R::cypher, query=query)
   }
   ##
   invisible(TRUE)
}

#' Feeding DODO: Register database in DODO DB
#'
#' Not exported to avoid unintended modifications of the DB.
#'
#' @param toImport a data.frame with the following columns:
#' - name: *character* name of the database
#' - idURL: *character* URL template for database concepts
#'
load_db_definition <- function(toImport){
   
   ## Checks ----
   tlc <- c("name"="character", "idURL"="character")
   mandatory <- c("name", "idURL")
   check_df_to_import(toImport, tlc, mandatory)
   
   ## Query ----
   cql <- c(
      'MERGE (db:Database {name:row.name})',
      'SET db.idURL=row.idURL'
   )
   import_in_dodo(neo2R::prepCql(cql), toImport)
}

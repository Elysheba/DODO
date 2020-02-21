###############################################################################@
#' Feeding DODO: Register databases with URL template in DODO DB
#'
#' Not exported to avoid unintended modifications of the DB.
#'
#' @param toImport a data.frame with the following columns:
#' - name: *character* name of the database (mandatory)
#' - idURL: *character* URL template for database concepts (optional)
#'
load_db_definitions <- function(toImport){
   ## Checks ----
   tlc <- c("name"="character", "idURL"="character")
   mandatory <- c("name")#, "idURL")
   check_df_to_import(toImport, tlc, mandatory)
   
   ## Query ----
   cql <- c(
      'MERGE (db:Database {name:row.name})',
      'SET db.idURL=row.idURL'
   )
   import_in_dodo(neo2R::prepCql(cql), toImport)
}

###############################################################################@
#' Feeding DODO: Register databases in DODO DB
#'
#' Not exported to avoid unintended modifications of the DB.
#'
#' @param toImport a data.frame with the following columns:
#' - name: *character* name of the database (mandatory)
#'
load_db_names <- function(toImport){
   ## Checks ----
   tlc <- c("name"="character")
   mandatory <- c("name")
   neoDODO:::check_df_to_import(toImport, tlc, mandatory)
   
   ## Query ----
   cql <- c(
      'MERGE (db:Database {name:row.name})'
   )
   neoDODO:::import_in_dodo(cql = neo2R::prepCql(cql), toImport = toImport)
}

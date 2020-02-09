###############################################################################@
#' Call a function on the DODO graph
#'
#' @param f the function to call
#' @param ... params for f
#' @param dodoCheck check if a connection to DODO exists (default: FALSE).
#'
#' @return The output of the called function.
#'
#' @seealso [check_dodo_connection]
#'
#' @export
#'
call_dodo <- function(f, ..., dodoCheck=FALSE){
   if(dodoCheck) if(!check_dodo_connection()){
      stop("No connection")
   }
   do.call(f, list(graph=get("graph", dodoEnv), ...))
}

###############################################################################@
#' Feeding DODO: Imports a data.frame in the DODO graph database
#'
#' Not exported to avoid unintended modifications of the DB.
#'
#' @param cql the CQL query to be applied on each row of toImport
#' @param toImport the data.frame to be imported as "row".
#' Use "row.FIELD" in the cql query to refer to one FIELD of the toImport
#' data.frame
#' @param ... additional parameters to the [neo2R::import_from_df()] function.
#'
#' @return the results of the query
#'
#' @seealso [call_dodo()]
#'
#' @importFrom utils write.table
#'
import_in_dodo <- function(
   cql, toImport, ...
){
   neo2R::import_from_df(graph=get("graph", dodoEnv), cql=cql, toImport, ...)
}

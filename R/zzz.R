#' 
#' @export
dodoEnv <- new.env(hash=TRUE, parent=emptyenv())

.onLoad <- function(libname, pkgname){
   connect_to_dodo()
}

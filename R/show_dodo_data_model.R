#' Show the data model of DODO
#'
#' Show the shema of the DODO data model.
#'
#' @export
#'
show_dodo_data_model <- function(){
   pkgname <- utils::packageName()
   htmlFile <- system.file(
      "Documentation", "DODO-Model", "DODO.html",
      package=pkgname
   )
   utils::browseURL(paste0('file://', htmlFile))
}

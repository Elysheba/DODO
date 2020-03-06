#========================================================================================@
#========================================================================================@
#' Split a disNet 
#' 
#' Splitting a disNet into a list of disNets based on a list of vectors containing disease identifiers. Generates a S3 object
#' *setDisNet*.
#' 
#' @param disNet the disease network to split
#' @param  diseaseList list character vectors of disease ids (format "MONDO:0005027")
#'
#' @return A a list of normalized disease networks 
#' 
#' @export
#' 
split_disNet <- function(disNet, 
                         diseaseList){
  toRet <- lapply(diseaseList, filter_by_id, disNet = disNet)
  class(toRet) <- "setDisNet"
  return(toRet)
}

#========================================================================================@
#========================================================================================@
#' Check validity of setDisNet object
#' @export

is.setDisNet <- function(x, ...){
  inherits(x, "setDisNet")  
}

#========================================================================================@
#========================================================================================@
#' Prints a setDisNet object
#' @export

print.setDisNet <- function(x, ...){
  
  cat(format(x, ...), sep = "\n")
  
}

#========================================================================================@
#========================================================================================@
#' format disNet
format.setDisNet <- function(x, ...){
  
  toRet <-    paste("The setDisNet contains",
                    length(x),
                    "disNet clusters")
  return(toRet)
}

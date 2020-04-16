###############################################################################@
#' Filter a disNet by database
#' 
#' Filter a disNet to keep only identifiers of databases of interest
#' 
#' @param disNet a disease network
#' @param databases a character vector of databases of interest (format: c("MONDO","EFO"))
#' 
#' @return A normalized disease network 
#' 
#' @examples 
#' disNet <- build_disNet(term = "epilep")
#' filtDisNet <- filter_by_database(disNet, databases = "EFO")
#' 
#' @export
#' 
filter_by_database <- function(disNet, databases){
  toRet <- disNet
  toRet$nodes <- disNet$nodes[which(disNet$nodes$database %in% databases),]
  return(normalize_disNet(toRet))
}

###############################################################################@
#' Filter a disNet by identifiers
#' 
#' Filtering a disNet to keep only diseases of interest
#' 
#' @param disNet a disease network
#' @param diseaseID a character vector of disease ids of interest (format: "MONDO:0005027")
#' 
#' @return A normalized disease network 
#' 
#' @examples
#' disNet <- build_disNet(term = "epilep")
#' filtDisNet <- filter_by_id(disNet, diseaseID = c("MONDO:0005027", "EFO:0000474"))
#' 
#' @export
#'

filter_by_id <- function(disNet, diseaseID){
  toRet <- disNet
  toRet$nodes <- disNet$nodes[which(disNet$nodes$id %in% diseaseID),]
  return(normalize_disNet(toRet))
}

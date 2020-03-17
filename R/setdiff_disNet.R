###############################################################################@
#' Substract one disease network from another
#' 
#' Splitting a disNet object into multiple subsets based the provided list 
#' containing vectors of disease identifiers. This generates a new type of S3 object: *setDisNet* object. 
#' Note that only the provided disease identifiers will be included into the setDisNet objects, all others 
#' are removed from the object. 
#' 
#' @param disNet1 the original disease network
#' @param disNet2 the disease network to substract
#' @param keepAllNodes if FALSE (default) all nodes of disNet2 are substracted.
#' If TRUE all nodes of disNet1 are kept even if not involved in final children
#' or xref relationships.
#' @param keepAllSeed if TRUE (default) all seed are kept in nodes after
#' filtering.
#'
#' @return A normalized disease network 
#' 
#' @export
#' 
setdiff_disNet <- function(disNet1, 
                          disNet2
                          # keepAllNodes = FALSE, 
                          # keepAllSeed = TRUE
){
  disNet <- disNet1
  disNet$nodes <- disNet$nodes[-which(disNet$nodes$id %in% disNet2$nodes$id),]
  return(normalize_disNet(disNet))
}

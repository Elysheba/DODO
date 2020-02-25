###############################################################################@
#' Normalize disease network
#' 
#' This function ensure that disease network properties are fulfilled.
#' 
#' @param disNet a disease network to normalize
#' 
#' @return A normalized disease network, disNet
#' 
#' @export
#' 
normalizeDisNet <- function(disNet){
  if(!is.null(disNet$children)){
    disNet$children <- disNet$children[which(
      disNet$children$parent %in% disNet$nodes$id &
        disNet$children$child %in% disNet$nodes$id
    ),]
  }
  if(!is.null(disNet$xref)){
    disNet$xref <- disNet$xref[which(!duplicated(disNet$xref$ur)),]
    disNet$xref <- disNet$xref[which(
      disNet$xref$from %in% disNet$nodes$id &
        disNet$xref$to %in% disNet$nodes$id
    ),]
  }
  if(!is.null(disNet$pheno)){
    # disNet$pheno <- disNet$pheno[which(!duplicated(disNet$pheno$ur)),]
    disNet$pheno <- disNet$pheno[which(
      disNet$pheno$disease %in% disNet$nodes$id &
        disNet$pheno$phenotype %in% disNet$nodes$id
    ),]
  }
  disNet$synonyms <- disNet$synonyms[which(
    disNet$synonyms$id %in% disNet$nodes$id
  ),]
  disNet$seed <- intersect(disNet$seed, disNet$nodes$id)
  return(disNet)
}


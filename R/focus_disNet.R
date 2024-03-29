###############################################################################@
#' Focus a disNet on identifiers of interest and neighbors
#' 
#' Focussing on particular identifiers of interest while also extending the disNet using the identifiers 
#' of interest as a starting point. Hierarchical or cross-reference edges are traversed that exist within the original
#' disNet. This doesn't require a connection to a database instance and will only consider the edges available in the
#' provided disNet object.
#' 
#' @param disNet a disease network
#' @param diseaseID a character vector of disease ids of interest (format "MONDO:0005027")
#' @param relationship  a character vector of the relationships to take into account. Can take values "xref","child","parent. 
#' Default is "xref" and "child".
#' @param steps the maximum number of steps between diseases of interest
#' and their neighbors (default: 2)
#' 
#' @return A normalized disease network 
#' 
#' @examples 
#' disNet <- build_disNet(term = "epilepsy")
#' focusDisNet <- focus_disNet(disNet = disNet, diseaseID = "EFO:0000474", steps = 2)
#' 
#' @export
#' 
focus_disNet <- function(disNet, 
                         diseaseID, 
                         relationship = c("xref","child"),
                         steps = 1
){
  match.arg(relationship, c("xref","child","parent"),several.ok = T)
  edges <- data.frame()
  if("child" %in% relationship){
    edges <- rbind(edges, 
                   dplyr::select(disNet$children,
                                 from = parent,
                                 to = child))
    }
  if("parent" %in% relationship){
    edges <- rbind(edges, 
                   dplyr::select(disNet$children,
                                 from = child,
                                 to = parent))
  }
  if("xref" %in% relationship){
    edges <- rbind(edges, 
                   disNet$xref[, c("from", "to")],
                   dplyr::select(disNet$xref,
                                 from = to,
                                 to = from))
  }
  toTake <- diseaseID
  nsteps <- 0
  taken <- edges[c(), ]
  while (nsteps < steps & nrow(taken) < nrow(edges)) {
    taken <- edges[which(edges$from %in% toTake), ]
    toTake <- unique(c(taken$from, taken$to,toTake))
    nsteps <- nsteps + 1
  }
  takenDis <- unique(c(taken$from, taken$to, toTake))
  return(normalize_disNet(filter_by_id(disNet, takenDis)))
}


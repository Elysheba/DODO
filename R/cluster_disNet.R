###############################################################################@
#' Cluster a disNet
#' 
#' Cluster a disNet object based on cross-references and/or parend/child information.  
#' 
#' @param disNet the disease network to cluster and split
#' @param clusterOn clustering taking either or both descendants and cross-references (values = "children","xref)
#' @param ambiguity level of backward ambiguity allowed
#' (default: 1 ==> no ambiguity allowed) 
#' @return A list of normalized disease networks: setDisNet object
#' 
#' @details 
#' Cluster a disNet way generate a *setDisNet* object. Based on the *igraph* package to identify 
#' connected components in a graph. These connected identifiers can identified using 
#' cross-reference or hierarchical edges.
#' 
#' @examples 
#' disNet <- build_disNet(term = "psoriasis")
#' setDisNet <- cluster_disNet(disNet = disNet, clusterOn = "xref")
#' 
#' @seealso \code{\link{build_disNet}}
#' @export
#' 
cluster_disNet <- function(disNet, 
                           clusterOn = c("xref"),
                           ambiguity = 1
                           ){
  match.arg(clusterOn, 
            c("children","xref"),
            several.ok = T)
  stopifnot(is.disNet(disNet))
  
  if("children" %in% clusterOn){
    child <- igraph::graph.data.frame(
      d = dplyr:: select(disNet$children,
                         from = parent,
                         to = child),
      directed=FALSE) %>% 
      igraph::as.directed(mode = "mutual")
  }
  
  if("xref" %in% clusterOn){
    tmp <- dplyr::select(disNet$xref,
                         from,
                         to,
                         forwardAmbiguity = forwardAmbiguity,
                         backwardAmbiguity = backwardAmbiguity) %>%
      dplyr::bind_rows(
        dplyr::select(disNet$xref,
                      from = to,
                      to = from,
                      forwardAmbiguity = backwardAmbiguity,
                      backwardAmbiguity = forwardAmbiguity))
    tmp <- tmp %>%
      dplyr::mutate(database = gsub(":.*", "", from)) %>%
      dplyr::distinct()
    
    xref <- igraph::graph.data.frame(
      d = tmp,
      directed = TRUE, 
      vertices = tmp %>% 
        dplyr::select(id = from, database) %>% 
        dplyr::distinct()
    )
    ## Filter ambiguities
    xref <- igraph::delete_edges(xref, 
                                 which(#igraph::E(xref)$forwardAmbiguity > forwardAmbiguity | 
                                         igraph::E(xref)$backwardAmbiguity > ambiguity))

  }
  
  if(length(clusterOn) == 2){
    pIgraph <- igraph::graph.union(child, xref)
  }else if (clusterOn == "children"){
    pIgraph <- child
  }else{
    pIgraph <- xref
  }
  
  ## Clustering
  cpIgraph <- igraph::clusters(pIgraph)

  ## Output clustering + add the blacklisted IDs back into each cluster
  if(cpIgraph$no == 0){
    toRet <- "NULL"
  }else if(cpIgraph$no == 1){
    cpIgraph <- names(cpIgraph$membership)
    toRet <- list("1" = filter_by_id(disNet, cpIgraph))
    class(toRet) <- "setDisNet"
  }else{
    missing <- disNet$nodes %>% 
      dplyr::filter(!id %in% names(cpIgraph$membership)) %>%
      dplyr::pull(id)
    missing <- sapply(missing, function(x){x}, USE.NAMES = FALSE, simplify = FALSE)
    ##
    cpIgraph <- utils::unstack(
      data.frame(
        n=names(cpIgraph$membership),
        c=cpIgraph$membership,
        stringsAsFactors=FALSE
      ),
      n~c
    )
    cpIgraph <- c(cpIgraph,
                  missing)
    names(cpIgraph) <- 1:length(cpIgraph)
    toRet <- split_disNet(disNet = disNet,
                          diseaseList = cpIgraph)
  }
  return(toRet)
}

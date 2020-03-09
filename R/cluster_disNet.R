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
#' setDisNet <- clusterDisNet(extDisNet,clusterOn = "xref")
#' 
#' @seealso buildDisNet
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
                                 which(igraph::E(xref)$forwardAmbiguity > forwardAmbiguity | 
                                         igraph::E(xref)$backwardAmbiguity > backwardAmbiguity))
    ## Filter blacklist
    # if(!all(is.na(avoidDB))){
    #   edgeList <- igraph::as_edgelist(xref) %>%
    #     tibble::as_tibble() %>%
    #     dplyr::rename(from = V1,
    #                   to = V2) %>%
    #     dplyr::mutate(edge = paste(from, to, sep = "|"))
    #   toRem <- edgeList %>%
    #     dplyr::filter(grepl(paste(avoidDB, collapse = "|"),
    #                         dplyr::pull(edgeList, edge))) 
    #   toKeep <- edgeList %>%
    #     dplyr::filter(!edge %in% toRem$edge)
    #   xref <- igraph::induced_subgraph(xref, 
    #                                    vids = which(igraph::V(xref)$name %in% toKeep$from))
    # }
  }
  
  if(length(clusterOn) == 2){
    pIgraph <- graph.union(child, xref)
  }else if (clusterOn == "children"){
    pIgraph <- child
  }else{
    pIgraph <- xref
  }
  
  ## Clustering
  cpIgraph <- igraph::clusters(pIgraph)
  
  # V(pIgraph)$color <- cpIgraph$membership+1
  # pIgraph <- set_graph_attr(pIgraph, "layout", layout_with_kk(pIgraph))
  # plot(pIgraph, layout=layout_nicely, vertex.label.dist=2)
  # 
  ## Output clustering + add the blacklisted IDs back into each cluster
  if(cpIgraph$no == 0){
    toRet <- "NULL"
  }else if(cpIgraph$no == 1){
    cpIgraph <- names(cpIgraph$membership)
    # if(any(cpIgraph %in% toRem$to)){
    #   cpIgraph <- c(cpIgraph, unique(toRem$from[which(toRem$to %in% cpIgraph)]))
    # }
    toRet <- list("1" = filter_by_id(disNet, cpIgraph))
    class(toRet) <- "setDisNet"
  }else{
    cpIgraph <- unstack(
      data.frame(
        n=names(cpIgraph$membership),
        c=cpIgraph$membership,
        stringsAsFactors=FALSE
      ),
      n~c
    )
    # cpIgraph <- sapply(cpIgraph,
    #                    function(x){
    #                      if(any(x %in% toRem$to)){
    #                        x <- c(x, unique(toRem$from[which(toRem$to %in% x)]))
    #                      }
    #                      return(x)
    #                    })
    toRet <- split_disNet(disNet = disNet,
                          diseaseList = cpIgraph)
  }
  return(toRet)
}

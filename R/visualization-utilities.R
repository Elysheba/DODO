#========================================================================================@
#========================================================================================@
#' Identify and show all relationships 
#' 
#' Takes the dataframe that was obtained from [DODO::convert_concept] as input and will query all cross-reference edges. 
#' For use of forward and backward ambiguity as well as black- and whitelist, see extendDisNet.
#' 
#' The initial identifier is represented by a triangle, each node is colored based on the database. The edges
#' between nodes are colored based on the ambiguity equal to one or larger.
#' 
#' @param df Dataframe. Dataframe with columns dbid1, type, dbid2, confidence
#' 
#' @details 
#' The edges are formatted based on forward ambiguity and type of the edge. 
#' The colour of the edges is based on ambiguity equal to one or larger than one.
#' This information is encoded when hovering over the edge.
#' The solid lines are used for *is_xref* edges, while dashed lines are
#' used from *is_related* edges.
#' 
#' 
#' @export

plot_relations <- function(df,
                           step = NULL,
                           transitive.ambiguity = 1, 
                           intransitive.ambiguity = NULL
                           ){
  if(length(id) > 1){
    stop("Too many ids provided")
  }
  dbid <- id
  ## Extend
  xref <- extend_disNet(build_disNet(id = dbid), 
                        relations = "xref", 
                        step = step,
                        transitive.ambiguity = transitive.ambiguity, 
                        intransitive.ambiguity = intransitive.ambiguity)
  col <- DODO:::color_database(disNet = xref)
  
  ## network object
  nodes <- xref$nodes %>%
    dplyr::select(id, 
                  database,
                  label) %>%
    dplyr::mutate(shape = dplyr::case_when(id %in% dbid ~ "triangle",
                                           TRUE ~ "dot"),
                  color = col[database],
                  title = paste(id, label, sep = " | "),
                  label = id) 
  edges1 <- xref$xref %>%
    dplyr::select(to, 
                  from,
                  ambiguity = forwardAmbiguity, 
                  type) %>%
    dplyr::mutate(color = dplyr::case_when(ambiguity > 1 ~ "#fdbb84",
                                           TRUE ~ "#7fcdbb"),
                  title = paste("Forward ambiguity = ", ambiguity),
                  type = gsub("_nba", "", type),
                  dashes = dplyr::case_when(type == "is_xref" ~ FALSE,
                                     TRUE ~ TRUE),
                  arrows = "to") %>%
    dplyr::distinct()
  edges2 <- xref$xref %>%
    dplyr::select(to = from, 
                  from = to,
                  ambiguity = backwardAmbiguity,
                  type) %>%
    dplyr::mutate(color = dplyr::case_when(ambiguity > 1 ~ "#fdbb84",
                                           TRUE ~ "#7fcdbb"),
                  title = paste("Forward ambiguity = ", ambiguity),
                  type = gsub("_nba", "", type),
                  dashes = dplyr::case_when(type == "is_xref" ~ FALSE,
                                     TRUE ~ TRUE),
                  arrows = "to") %>%
    dplyr::distinct()
  edges <- dplyr::bind_rows(edges1,
                            edges2) %>%
    dplyr::distinct()
  
  if(nrow(nodes) > 200){
    message(paste(nrow(nodes), " nodes, displaying as igraph layout"))
    visNetwork::visNetwork(nodes = nodes, edges = edges) %>%
      visNetwork::visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE),
                             collapse = list(enabled = TRUE)) %>%
      visNetwork::visIgraphLayout()
  }else{
    visNetwork::visNetwork(nodes = nodes, edges = edges) %>%
      visNetwork::visPhysics(barnesHut = list(damping = 0.5), stabilization = FALSE) %>%
      visNetwork::visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 2),
                             collapse = list(enabled = TRUE))
  }
}


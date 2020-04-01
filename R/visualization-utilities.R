#========================================================================================@
#========================================================================================@
#' Visualize a disease network
#' 
#' Visualize a disNet or setDisNet object as a visNetwork plot. It can be specified to 
#' show only a particular edge type 
#' and by default only 500 disease nodes are plotted.
#' 
#' @param disNet a disease network
#' @param relations relationships to plot, can take "xref","child" and/or "parent" as values
#' @param number Number of nodes to plot without igraph. If the number of nodes exceeds, 
#' igraph will be used (integer)
#' @param igraph use visIgraphLayout() (default = TRUE)
#' @param labelling use either disease identifier or both disease identifier and label 
#' to label nodes (default = FALSE)
#'  
#' @return A \code{\link{visNetwork}} object.
#' 
#' @examples 
#' disNet <- extendDisNet(buildDisNetByID(id = "MONDO:0005027"))
#' plot.disNet(disNet) 
#' 
#' @export
#' 
plot.disNet <- function(
  disNet,
  relations = c("xref","child","parent"),
  number = 500,
  igraph = T,
  labelling = FALSE,
  ...
){
  match.arg(relations, 
            c("xref","child","parent"),
            several.ok = T)
  match.arg(label, c("id", "label"), several.ok = FALSE)
  stopifnot(is.disNet(disNet))
  
  ## Colors
  col <- DODO:::color_database(disNet = disNet)
  
  ## Edges
  edges <- tibble(from = character(),
                  to = character(),
                  title = character(),
                  arrows = character())
  if("xref" %in% relations && !is.null(disNet$xref)){
    edges <- disNet$xref %>%
      dplyr::select(from, 
                    to,
                    type,
                    FA = forwardAmbiguity,
                    BA = backwardAmbiguity) %>%
      dplyr::mutate(color = dplyr::case_when(grepl("is_related", type) ~ "#fc8d62",
                                             TRUE ~ "#8da0cb"),
                    title = paste(gsub("_nba", "", type), ", FA = ", FA, ", BA = ", BA),
                    arrows = case_when(FA == 1 & BA == 1 ~ "to;from",
                                       BA == 1 ~ "from",
                                       FA == 1 ~ "to",
                                       TRUE ~ "FALSE"))
  }
  if(any(c("child","parent") %in% relations) && !is.null(disNet$children)){
    edges <- disNet$children %>%
      dplyr::select(from = parent, 
                    to = child,
                    origin) %>%
      dplyr::mutate(title = paste("is_a, origin = ", origin),
                    arrows = "to",
                    color = "#66c2a5") %>%
      dplyr::bind_rows(edges)
  }
  
  ## Nodes
  nodes <- disNet$nodes %>%
    dplyr::select(id,
                  database,
                  label) %>%
    dplyr::mutate(color = col[database],
                  shape = case_when(id %in% disNet$seed ~ "triangle",
                                    TRUE ~ "dot"),
                  lbl = label,
                  label = case_when(labelling ~ paste(id, lbl, sep = " | "),
                                    TRUE ~ id),
                  title = paste(id, lbl, sep = " | ")) 
  
  
  ## Visnetwork
  if(nrow(nodes) > number || igraph){
    message(paste("Plotting", nrow(nodes),"nodes"))
    visNetwork::visNetwork(nodes = nodes, 
                           edges = edges) %>%
      visNetwork::visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE),
                             collapse = list(enabled = TRUE)) %>%
      visNetwork::visIgraphLayout(smooth = TRUE)
  }else{
    message(paste("Plotting", nrow(nodes),"nodes"))
    visNetwork::visNetwork(nodes = nodes, edges = edges) %>%
      visNetwork::visPhysics(barnesHut = list(damping = 0.1), stabilization = FALSE) %>%
      visNetwork::visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 2),
                             collapse = list(enabled = TRUE))
  }
}


#========================================================================================@
#========================================================================================@
#' Visualize one cluster of set disease networks
#' 
#' Visualize one cluster of a setDisNet object as a visNetwork plot. It can be specified to show only a particular edge type 
#' and by default only 500 disease nodes are plotted.
#' 
#' @param disNet a disease network
#' @param relations relationships to plot, can take "xref","child" and/or "parent" as values
#' @param cluster cluster to plot
#' @param number Number of nodes to plot without igraph. If the number of nodes exceeds, igraph will be used (integer)
#' @param igraph use visIgraphLayout() (default = TRUE)
#' @param labelling use either disease identifier or both disease identifier and label 
#' to label nodes (default = FALSE)
#' 
#' @return A \code{\link{visNetwork}} object.
#' 
#' @examples 
#' disNet <- clusterDisNet(extendDisNet(buildDisNetByTerm(id = "ALS")), clusterOn = "xref")#' 
#' plot(disNet, cluster = 1) 
#' 
#' @export
#' 
plot.setDisNet <- function(
  disNet,
  relations = c("xref","child","parent"),
  cluster = NULL,
  number = 500,
  igraph = T,
  labelling = FALSE,
  ...
){
  match.arg(relations, 
            c("xref","child","parent"),
            several.ok = T)
  stopifnot(is.setDisNet(disNet))
  if(is.null(cluster)) stop("Please provide a cluster to plot ...")
  disNet <- disNet[[cluster]]
  
  plot.disNet(disNet = disNet, 
              relations = relations, 
              number = number, 
              igraph  = igraph,
              labelling = labelling)
}

#========================================================================================@
#========================================================================================@
#' Identify and show all relationships 
#' 
#' Takes a identifier (full length, e.g. "MONDO:0005027") as input and will query all cross-reference edges. 
#' For use of forward and backward ambiguity as well as black- and whitelist, see extendDisNet.
#' 
#' The initial identifier is represented by a triangle, each node is colored based on the database. The edges
#' between nodes are colored based on the ambiguity equal to one or larger.
#' 
#' @param dbid concept identifier formatted as db:id
#' @param relations the kind of relationships to take into account -> xref
#' @param step the depth of the search (default: 10000 ==> ~exhaustive)
#' @param transitive.ambiguity backward ambiguity while using transitivity to identify cross-references (default: 1)
# @param FA.non_transitivity forward ambiguity while using transitivity to identify cross-references (default: no filter)
#' @param intransitive.ambiguity backward ambiguity while using transitivity to identify cross-references (default: no filter)
#' @param blacklist a vector of databases to avoid when extending (default = NA)
#' @param whitelist a vector of databases to trust when extending, only going through these database nodes (default = NA) 
#' 
#' @details 
#' The edges are formatted based on forward ambiguity and type of the edge. 
#' The colour of the edges is based on ambiguity equal to one or larger than one.
#' This information is encoded when hovering over the edge.
#' The solid lines are used for *is_xref* edges, while dashed lines are
#' used from *is_related* edges.
#' 
#' @seealso extendDisNet
#' 
#' @export

show_relations <- function(id,
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
                  dashes = case_when(type == "is_xref" ~ FALSE,
                                     TRUE ~ TRUE),
                  arrows = "to") %>%
    distinct()
  edges2 <- xref$xref %>%
    dplyr::select(to = from, 
                  from = to,
                  ambiguity = backwardAmbiguity,
                  type) %>%
    dplyr::mutate(color = dplyr::case_when(ambiguity > 1 ~ "#fdbb84",
                                           TRUE ~ "#7fcdbb"),
                  title = paste("Forward ambiguity = ", ambiguity),
                  type = gsub("_nba", "", type),
                  dashes = case_when(type == "is_xref" ~ FALSE,
                                     TRUE ~ TRUE),
                  arrows = "to") %>%
    distinct()
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

#========================================================================================@
#========================================================================================@
#' Database colours 
#' 
#' Function to return set colours for disease ontology resources
#' 
color_database <- function(disNet){
  ## file
  dodoDir <- file.path(Sys.getenv("HOME"),"R","DODO")
  colFile <- file.path(dodoDir,
                       "DODO-Colors.rda")
  ## colors
  if(file.exists(colFile)){
    load(colFile)
    if(any(!disNet$nodes$database %in% names(col))){
      col <- unique(c(RColorBrewer::brewer.pal(n = 8, name = "Dark2"), 
                      RColorBrewer::brewer.pal(n = 8, name = "Set1"),
                      RColorBrewer::brewer.pal(n = 8, name = "Set2"),
                      RColorBrewer::brewer.pal(n = 8, name = "Set3"),
                      RColorBrewer::brewer.pal(n = 8, name = "Accent"),
                      RColorBrewer::brewer.pal(n = 8, name = "Pastel2"),
                      RColorBrewer::brewer.pal(n = 8, name = "Pastel1"),
                      RColorBrewer::brewer.pal(n = 8, name = "Paired")))
      db <- list_database()
      col <- col[1:nrow(database)]
      names(col) <- pull(database, database)
      save(col, file = colFile)
    }
  }else{
    col <- unique(c(RColorBrewer::brewer.pal(n = 8, name = "Dark2"), 
                    RColorBrewer::brewer.pal(n = 8, name = "Set1"),
                    RColorBrewer::brewer.pal(n = 8, name = "Set2"),
                    RColorBrewer::brewer.pal(n = 8, name = "Set3"),
                    RColorBrewer::brewer.pal(n = 8, name = "Accent"),
                    RColorBrewer::brewer.pal(n = 8, name = "Pastel2"),
                    RColorBrewer::brewer.pal(n = 8, name = "Pastel1"),
                    RColorBrewer::brewer.pal(n = 8, name = "Paired")))
    db <- list_database()
    col <- col[1:nrow(db)]
    names(col) <- pull(db, database)
    save(col, file = colFile)
  }
  return(col)
}

#========================================================================================@
#========================================================================================@
#' Explore diseases in a disease network
#' 
#' Explore a disNet by a datatable highlighting a particular 
#' term within the information provided by label, synonym or definition. 
#' 
#' @param disNet a disease network
#' @param show the description to be returned (label, synonym, definition) 
#' @param terms the terms to be highlighted
#' 
#' @return A \code{\link{datatable}} object.
#' 
#' @examples 
#' exploreDisNet(extDisNet, show = c("label"), terms = "epilep")
#' 
#' @export
#' 
explore_disNet <- function(disNet, 
                          show = "label",
                          terms = NULL,
                          ...){
  show <- match.arg(show,c("label","synonym","definition"),several.ok = F)
  stopifnot(is.disNet(disNet) || is.setDisNet(disNet))
  
  if(is.disNet(disNet)){
    if(show == "label"){
      toShow <- disNet$nodes
      toShow$values <- toShow$label
      toShow$values <- highlightText(toShow$label, ifelse(length(terms) == 1, terms, paste(terms,collapse="|")))
    }else if(show == "synonym"){
      toShow <- disNet$synonyms
      if(nrow(toShow) > 1 & nrow(toShow) > length(unique(toShow$id))){
        if(length(unique(toShow$id)) > 1){
          toShow <- unstack(x = toShow, synonym ~ id)
          toShow <- lapply(toShow,function(x) paste(x,collapse = " <br> "))
          toShow <- stack(toShow)
          toShow <- toShow[,c(2,1)]
        }else{
          toShow <- data.frame(ind = unique(toShow$id),
                               values = paste(toShow$synonym,collapse = " <br> "),
                               stringsAsFactors = F)
        }
      }
      names(toShow) <- c("id","values")
      toShow$id <- as.character(toShow$id)
      toShow$values <- highlightText(toShow$values, ifelse(length(terms) == 1, terms, paste(terms,collapse="|")))
    }else{
      toShow <- disNet$nodes
      toShow$values <- toShow$definition
      toShow$values <- highlightText(toShow$definition, ifelse(length(terms) == 1, terms, paste(terms,collapse="|")))
    }
    print(DT::datatable(toShow[,c("id","values")],
                        colnames = c("Disease ID",show),
                        rownames = TRUE,
                        options = list(order = list(0,'asc')),
                        escape=FALSE,
                        ...))
  }else{
    counter <- 1
    toShow <- do.call(rbind,
                      lapply(disNet,
                             function(x){
                               toRet <- x$nodes %>%
                                 dplyr::arrange(level) %>%
                                 dplyr::mutate(clusterSize = length(id)) %>%
                                 dplyr::slice(1) %>%
                                 dplyr::mutate(values = case_when(show == "label" ~ label,
                                                                  show == "definition" ~ definition,
                                                                  TRUE ~ NA_character_),
                                               cluster = counter)
                               if(show == "synonym"){
                                 values <- x$synonyms %>%
                                   dplyr::filter(id == dplyr::pull(toRet, id)) %>%
                                   dplyr::pull(synonym) %>%
                                   paste(collapse = "<br>")
                                 toRet <- toRet %>%
                                   dplyr::mutate(values = values)
                               } 
                               toRet <- toRet %>%
                                 dplyr::select(cluster, clusterSize, id, level, values) %>%
                                 dplyr::mutate(values = highlightText(toRet$values,
                                                                      ifelse(length(terms) == 1, 
                                                                             terms, 
                                                                             paste(terms,collapse="|"))
                                 )
                                 )
                               counter <<- counter + 1
                               return(toRet)
                             })
    )
    print(DT::datatable(toShow[,c("cluster", "clusterSize", "id", "level", "values")],
                        # colnames = c("cluster","id",show),
                        rownames = FALSE,
                        escape=FALSE,
                        filter = "top",
                        ...))
  }
}

#========================================================================================@
#========================================================================================@
#' Highlight function 
#' 
#' Highlighting a term in a text with HTML
#' 
#' 
highlightText <- function(text, value){
  if(is.null(value) || value == ""){
    return(text)
  }
  return(unlist(lapply(
    text,
    function(x){
      if(is.na(x)){
        return(x)
      }
      p <- gregexpr(value, x, ignore.case=TRUE)[[1]]
      if(p[1]>0){
        toRet <- c(substr(x, 0, p[1]-1))
        for(i in 1:length(p)){
          toRet <- c(
            toRet,
            '<mark style="background-color:yellow;font-weight:bold;">',
            substr(x, p[i], p[i]+attr(p, "match.length")[i]-1),
            '</mark>',
            substr(
              x,
              p[i]+attr(p, "match.length")[i],
              min(
                p[i+1]-1,
                nchar(x)+1,
                na.rm=TRUE
              )
            )
          )
        }
        toRet <- paste(toRet, collapse="")
      }else{
        toRet <- x
      }
      return(toRet)
    }
  )))
}

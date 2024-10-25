# ========================================================================================@
# ========================================================================================@
#' Identify and show all relationships
#'
#' Takes the datatable that was obtained from [DODO::get_network] as input and
#' visualizes the nodes and edges as a visnetwork
#'
#' @param ids String. A vector with identifier to convert formatted as DB:id (eg. "MONDO:0005027)
#' @param relationship String. Type of relationship to extract ("xref", "parent", "child",
#' "phenotype", "disease", "alternative"). Default: xref.
#' @param direct Boolean. Only getting direct relationships or moving through the disease
#' network to return all connected relationship.
#' @param sep String. Separator used, default: ":".
#' @param verbose show query input (default = FALSE)
#'
#' @export
#' @import data.table
#' @import magrittr
#' @import visNetwork
plot_relations <- function(ids,
                           relationship = c("xref"),
                           direct = F,
                           sep = ":",
                           verbose = FALSE) {
  checkmate::assertNames(relationship, subset.of = c(
    "xref", "parent", "child",
    "phenotype", "disease",
    "alternative"
  ))
  checkmate::assertLogical(direct)

  ids <- DODO:::check_divider(ids, sep = sep, format = "in")

  toPlot <- get_network(
    ids = ids,
    relationship = relationship,
    direct = direct,
    verbose = verbose
  )

  # Extract the prefix for coloring nodes
  toPlot[, `:=`(
    DB_from = sub("\\|.*|\\_.*", "", from),
    DB_to = sub("\\|.*|\\_.*", "", to),
    from = DODO:::check_divider(from, sep = sep, format = "out"),
    to = DODO:::check_divider(to, sep = sep, format = "out")
  )]

  # Create unique nodes
  nodes <- unique(data.table(
    id = c(toPlot$from, toPlot$to),
    prefix = c(toPlot$DB_from, toPlot$DB_to)
  ))

  # Define colors based on the prefix
  base_colors <- hcl.colors(length(unique(c(toPlot$DB_from, toPlot$DB_to))),
    palette = "Zissou 1"
  )
  colors <- setNames(
    adjustcolor(base_colors, alpha.f = 1),
    unique(c(toPlot$DB_from, toPlot$DB_to))
  )
  nodes[, `:=`(
    color = colors[prefix],
    label = id
  )]

  # Create edges without labels
  edges <- toPlot[, .(from, to)] %>%
    .[, arrows := "to"]

  # Plot with visNetwork
  p <- visNetwork(nodes, edges) %>%
    visLayout(randomSeed = 42) %>%
    visPhysics(stabilization = TRUE) %>%
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, hover = TRUE),
      collapse = list(enabled = TRUE)
    )
  if (nrow(nodes) > 200) {
    message(paste(nrow(nodes), " nodes, displaying as igraph layout"))
    p <- p %>%
      visNetwork::visIgraphLayout()
  }
  p
}

# ========================================================================================@
# ========================================================================================@
#' Connect to a neo4j DODO database
#'
#' @param local	Boolean. Connect to a local Neo4J instance, containing the Knowledge Graph. Default: FALSE.
#' @param user_name	String. The user name to connect to the Graph.
#' @param password	String. The password to connect to the Graph.
#' @param local_port	String. Which port is exposed to the local https:// connection. Default: 7474
#' @param azure_authorization Object. Location of the rds object with the azure token.
#' @param verbose	Boolean. Controls verbosity of the function.
#'
#' @return Established a connection to local or remote DODO instance
#'
#' @export
connect_to_dodo <- function(
    local = F,
    user_name = "",
    password = "",
    local_port = "7474",
    azure_authorization = NULL,
    verbose = T) {
  checkmate::qassert(local, "B1")
  checkmate::qassert(local_port, "S1")
  checkmate::qassert(verbose, "B1")
  graph <- if (local) {
    if (verbose) {
      message(
        "Connecting to a local Neo4J instance of the Knowledge Graph via port: ",
        local_port, "."
      )
    }
    neo2R::startGraph(paste0("http://localhost:", local_port),
      username = user_name,
      password = password,
      .opts = list(ssl_verifypeer = 0)
    )
  } else {
    if (verbose) {
      message("Connecting to a UCB Neo4J instance of the Knowledge Graph via https://dodo.ucb.com")
    }
    checkmate::assertFileExists(azure_authorization)
    token <- readRDS(azure_authorization)
    neo2R::startGraph(
      url = "https://dodo.ucb.com/",
      username = user_name,
      password = password,
      .opts = list(
        ssl_verifypeer = 0,
        extendedHeaders = DODO:::.get_tk_headers(token)
      )
    )
  }
  attr(graph, "neo2R") <- TRUE
  .dodo_env <<- new.env(parent = .GlobalEnv)
  .dodo_env$graph <- graph
  .dodo_env$dodo_version <- "2.0.0"
}

# ========================================================================================@
# ========================================================================================@
#' use azure token
#' 
#' @param token String. Azure token
#' @param always Boolean.
.get_tk_headers <- function(token, always = F){
  checkmate::assertClass(token, "AzureTokenAuthCode")
  checkmate::qassert(always, "B1")
  if (always || !token$validate()) 
    token$refresh()
  list(Authorization = paste("Bearer", token$credentials$access_token))
}

# ========================================================================================@
# ========================================================================================@
#' Check if there is a connection to a DODO database
#'
#' @param verbose if TRUE print information about the DODO connection
#' (default: TRUE).
#'
#' @return
#'  - TRUE if the connection can be established
#'  - Or FALSE if the connection cannot be established
#'
#' @seealso [connect_to_dodo]
#'
#' @export
#'
check_dodo_connection <- function(verbose = T) {
  if (!"graph" %in% names(.dodo_env)) {
    warning(
      "You should connect to a DODO DB using the connect_to_dodo function"
    )
    return(FALSE)
  } else {
    dbVersion <- try(call_dodo(
      f = neo2R::cypher,
      query = neo2R::prepCql(c(
        "MATCH (n:System) RETURN",
        "n.name as name, n.instance as instance, n.version as version"
      )),
      dodoCheck = FALSE
    ))
    if (verbose) message(.dodo_env$graph$url)
    if (verbose) message(dbVersion)
    return(TRUE)
  }
}

# ========================================================================================@
# ========================================================================================@
#' Call a function on the DODO graph
#'
#' @param f the function to call
#' @param ... params for f
#' @param dodoCheck check if a connection to DODO exists (default: FALSE).
#'
#' @return The output of the called function.
#'
#' @seealso [check_dodo_connection]
#'
#' @export
#'
call_dodo <- function(f, ..., dodoCheck = FALSE) {
  if (dodoCheck) {
    if (!check_dodo_connection()) {
      stop("No connection")
    }
  }
  do.call(f, list(graph = .dodo_env$graph, ...))
}

# ========================================================================================@
# ========================================================================================@
#' Show the data model of DODO
#'
#' Show the shema of the DODO data model.
#'
#' @export
#' 
#' @import data.table
#' @import magrittr
show_dodo_model <- function() {
  cql <- c("MATCH (n)-[r]->(n2) RETURN DISTINCT labels(n) as node1, type(r) as type, labels(n2) as node2")
  toRet <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    result = "row"
  ) %>%
    data.table::as.data.table() %>%
    .[!grepl(" ", node1) & !grepl(" ", node2)]

  # Create nodes and edges for visNetwork
  nodes <- data.table(
    id = unique(c(toRet$node1, toRet$node2)),
    label = unique(c(toRet$node1, toRet$node2)),
    color = c("seagreen", "tomato", "lightblue")
  )

  edges <- data.table(
    from = toRet$node1,
    to = toRet$node2,
    label = toRet$type
  )

  # Create the network visualization
  visNetwork::visNetwork(nodes, edges) %>%
    visNetwork::visEdges(arrows = "to") %>% # Adds arrow to edges
    visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visNetwork::visPhysics(
      solver = "barnesHut",
      barnesHut = list(avoidOverlap = 1, springLength = 400, enabled = FALSE)
    )
}

# ========================================================================================@
# ========================================================================================@
#' Helper to build queries for multicypher
#'
#' @param statements cypher query
#' @param result the way to return results. "row" will return a data frame
#' and "graph" will return a list of nodes, a list of relationships and
#' a list of paths (vectors of relationships identifiers).
#' @param parameters parameters for the cypher query
#'
build_multicypher <- function(statements,
                              result = "row",
                              parameters = NULL) {
  if (length(result) != length(statements)) {
    result <- rep(result, length(statements))
  }
  qs <- lapply(
    1:length(statements),
    function(s) {
      toRet <- list(
        statement = statements[[s]],
        resultDataContents = list(result[s])
      )
      if (!is.null(parameters)) {
        toRet$parameters <- parameters
      }
      return(toRet)
    }
  )
  names(qs) <- names(statements)
  return(qs)
}

# ========================================================================================@
# ========================================================================================@
#' helper function to deal with id construction
#'
#' This helper function will replace the original separator by a pipe and back.
#'
#' @param ids String. Vector of identifiers to check the format for
#' @param sep String. Separator used, default: ":".
#' @param format String. "in" for performing query in graph, "out" for converting back the
#'  identifiers to the user
#'
#' @return vector of identifier with DB and short identifier separated by pipe or back
#' to colon
check_divider <- function(ids, sep = ":", format = c("in", "out")) {
  checkmate::assertNames(format, subset.of = c("in", "out"))

  if (format == "in") {
    toRet <- gsub(sep, "|", ids)
  } else {
    toRet <- gsub("\\|", ":", ids)
  }
  return(toRet)
}

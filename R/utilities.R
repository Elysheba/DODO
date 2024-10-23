#========================================================================================@
#========================================================================================@
#' Connect to a neo4j DODO database
#'
#' @param local	Boolean. Connect to a local Neo4J instance, containing the Knowledge Graph. Default: FALSE.
#' @param user_name	String. The user name to connect to the Graph.
#' @param password	String. The password to connect to the Graph.
#' @param local_port	String. Which port is exposed to the local https:// connection. Default: 7474
#' @param ucb_http	String. Which http address is exposed to within the UCB instance for the https:// connection. Default: https://dodo.ucb.com/ 
#' @param verbose	Boolean. Controls verbosity of the function.
#'
#' @return Established a connection to local or remote DODO instance
#'
#'
#' @export
#'
connect_to_dodo <- function(
    local = F, 
    user_name = "", 
    password = "", 
    local_port = "7474", 
    ucb_http = "https://dodo.ucb.com/",
    azure_authorization = "/Local_Disk/Docker_Volumes/Secrets/kmt_authorization.rds",
    verbose = T) 
    {
      checkmate::qassert(local, "B1")
      checkmate::qassert(local_port, "S1")
      checkmate::qassert(verbose, "B1")
      graph <- if (local) {
        if (verbose) {
          message("Connecting to a local Neo4J instance of the Knowledge Graph via port: ", 
                  local_port, ".")
        }
        neo2R::startGraph(paste0("http://localhost:", local_port), 
                          username = user_name, 
                          password = password, 
                          .opts = list(ssl_verifypeer = 0))
      }
      else {
        if (verbose) {
          message("Connecting to a UCB Neo4J instance of the Knowledge Graph via https://dodo.ucb.com")
        }
        checkmate::assertFileExists(azure_authorization)
        token <- readRDS(azure_authorization)
        neo2R::startGraph(url = "https://dodo.ucb.com/", 
                          username = user_name, 
                          password = password, 
                          .opts = list(ssl_verifypeer = 0,
                                       extendedHeaders = Aether::.get_tk_headers(token)))
      }
      attr(graph, "neo2R") <- TRUE
      .dodo_env <<- new.env(parent = .GlobalEnv)
      .dodo_env$graph <- graph
      .dodo_env$dodo_version <- "2.0.0"
      # if (!.test_DODOversion()) {
      #   warning("You are not connected to the expected version of the graph, i.e., ", 
      #           .dodo_env$dodo_version)
      # }
}

#========================================================================================@
#========================================================================================@
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
check_dodo_connection <- function(verbose=T){
  if(!"graph" %in% names(.dodo_env)){
    warning(
      "You should connect to a DODO DB using the connect_to_dodo function"
    )
    return(FALSE)
  }else{
    dbVersion <- try(call_dodo(
      f=neo2R::cypher,
      query=neo2R::prepCql(c(
        'MATCH (n:System) RETURN',
        'n.name as name, n.instance as instance, n.version as version'
      )),
      dodoCheck=FALSE
    ))
    if(verbose) message(.dodo_env$graph$url)
    if(verbose) message(dbVersion)
    return(TRUE)
  }
}


#========================================================================================@
#========================================================================================@
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
call_dodo <- function(f, ..., dodoCheck=FALSE){
  if(dodoCheck) if(!check_dodo_connection()){
    stop("No connection")
  }
  do.call(f, list(graph=.dodo_env$graph, ...))
}

#========================================================================================@
#========================================================================================@
#' List of databases in DODO
#' 
#' lists all databases present in the database
#' 
#' @examples list_database()
#'
#' @return Returns a list of all database in DODO
#'
#' @export
#' @import data.table
list_database <- function(){
  ## Prepare query
  cql <- c('MATCH (n:Database)<-[r:is_in]-(d)',
           'RETURN n.db as database, count(r) as count')
  toRet <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    result = "row"
  ) %>% 
    data.table::as.data.table()
  return(toRet)
}

#========================================================================================@
#========================================================================================@
#' List of nodes types in DODO
#' 
#' Lists the different type of node present in the database
#' 
#' @examples list_node_type()
#'
#' @return Returns a list of all database in DODO
#'
#' @export
#' @import data.table
list_node_type <- function(){
  cql <- c('MATCH (n)',
           'RETURN labels(n) as type, count(n) as count')
  toRet <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    result = "row"
  ) %>%
    data.table::as.data.table()
  return(toRet)
}


#========================================================================================@
#========================================================================================@
#' Get concept description
#' 
#' Describing provided concept identifiers (when available) using disease label
#' 
#' @param ids a character vector of  concept identifier to search (e.g. "MONDO:0005027")
#' @param verbose return query content (default = FALSE)
#' @return vector of disease labels
#' 
#' @export
#' @import data.table
describe_concept <- function(ids, verbose = FALSE){
  ## Checking
  if(is.null(ids)){ 
    stop('Please provide ID')
  }
  
  cql <- c('MATCH (n)',
           'WHERE n.dbid IN $from',
           'RETURN n.dbid as dbid, n.name as label, n.def as definition')
  if(verbose){
    cat(cql, sep = "\n")
  }
  
  toRet <- call_dodo(
      neo2R::cypher,
      neo2R::prepCql(cql),
      parameters = list(from = as.list(ids)),
      result = "row")  %>% 
    data.table::as.data.table()
  
  return(toRet)
}


#========================================================================================@
#========================================================================================@
#' Get URL of database
#' 
#' Returns a vector of urls of the provided concept identifiers
#' 
#' @param ids concept identifiers as vector (e.g. "MONDO:0005027")
#' @param exact returns url of the original database (e.g. MONDO url for "MONDO:0005027"). Default = TRUE
#' 
#' @return Returns a database with id, origin, url, db, shortID
#'  
#' @examples
#' get_concept_url(ids = "MONDO:0005027")
#' 
#' @export
#' @import data.table
get_concept_url <- function(ids,
                           exact = TRUE){
  ## Checking
  if(is.null(ids)){ 
    stop('Please provide ID')
  }
  
  cql <- c('MATCH (n)-[r:is_in]->(db:Database)',
           'WHERE n.dbid IN $from',
           'RETURN n.dbid as dbid ,n.DB as db, n.id as id, db.db as origin, db.url as url')
  toRet <- call_dodo(
      neo2R::cypher,
      neo2R::prepCql(cql),
      parameters = list(from = as.list(ids)),
      result = "row") %>%
    as.data.table() %>% 
    .[, url := sprintf(stringr::str_replace(url, "%3A", "%%3A"), id)]
    
  if(exact){
    toRet <- toRet[db == origin,]
  }  
  return(toRet)
}


#========================================================================================@
#========================================================================================@
#' Returns current version
#' 
#' Returns the version of the current DODO database
#'
#' @examples get_version()
#'
#' @return Returns current version of DODO instance
#'
#' @export
#' @import data.table
get_version <- function(){
  cql <- c('MATCH (f {name: "DODO"})',
           'RETURN f.name as Name, f.instance as Instance, f.version as Version')
  toRet <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    result = "row"
  ) %>%
    data.table::as.data.table()
  return(toRet)
}

#========================================================================================@
#========================================================================================@
#' Show the data model of DODO
#'
#' Show the shema of the DODO data model.
#'
#' @export
#' @import data.table
show_dodo_model <- function(){
  cql <- c("MATCH (n)-[r]->(n2) RETURN DISTINCT labels(n) as node1, type(r) as type, labels(n2) as node2")
  toRet <- call_dodo(
      neo2R::cypher,
      neo2R::prepCql(cql),
      result = "row") %>%
    data.table::as.data.table() %>% 
    .[!grepl(" ", node1) & !grepl(" ", node2)]

  # Create nodes and edges for visNetwork
  nodes <- data.table(id = unique(c(toRet$node1, toRet$node2)),
                      label = unique(c(toRet$node1, toRet$node2)),
                      color = c("seagreen", "tomato", "lightblue"))
  
  edges <- data.table(from = toRet$node1,
                      to = toRet$node2,
                      label = toRet$type)
  
  # Create the network visualization
  visNetwork::visNetwork(nodes, edges) %>%
    visNetwork::visEdges(arrows = "to") %>% # Adds arrow to edges
    visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>% 
    visNetwork::visPhysics(solver = "barnesHut",
               barnesHut = list(avoidOverlap = 1, springLength = 400, enabled = FALSE))
}

#========================================================================================@
#========================================================================================@
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
                              parameters = NULL){
  if(length(result) != length(statements)){
    result <- rep(result, length(statements))
  }
  qs <- lapply(1:length(statements),
               function(s){
                 toRet <- list(statement = statements[[s]],
                               resultDataContents = list(result[s]))
                 if(!is.null(parameters)){
                   toRet$parameters <- parameters
                 }
                 return(toRet)
               })
  names(qs) <- names(statements)
  return(qs)
}


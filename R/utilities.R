#========================================================================================@
#========================================================================================@
# Check if there is a connection to a DODO database
#
# Checks if there is a connection already established with a DODO Dgraph instance
#
# @param verbose if TRUE print information about the DODO connection
# (default: FALSE).
#
# @return \itemize{
#  \item{TRUE if the connection can be established}
#  \item{Or FALSE if the connection cannot be established or the "System" node
#  does not exist or does not have "DODO" as name or any version recorded.
#  }
# }
#
#
# @export
# checkDODOConn <- function(verbose = FALSE){
#  if(!exists("graph", dodoEnv)){
#     warning("You should connect to a DODO DB using the connectToDODO function")
#     return(FALSE)
#   }
#   if(verbose) message(get("graph", dodoEnv)$url)
#   q <- c('{',
#          'dodo(func: eq(type, "DODO")){name version}',
#          '}')
#   q <- paste(q,collapse = "\n")
#   rq <- try(dodoCall(DgraphR::dgraphRequest,
#                      postText = q,
#                      dodoCheck=FALSE))
#   if(inherits(rq, "try-error")){
#     return(FALSE)
#   }else{
#     rq <- unlist(rq$result$data$dodo)
#     if(verbose){
#       message(paste("Version DODO:", rq["version"]))
#     }
#     return(TRUE)
#   }
# }

#========================================================================================@
#========================================================================================@
# Connect to DODO instance
#
# Connects to a DODO Dgraph instance by the default port "localhost:8080".
#
# @param host host for the graph database (default:"localhost")
# @param port port on which the graph database is listening (default: 8080)
# @param remember if TRUE the connection is stored to be used next time
# @param reconnect if TRUE it will use stored connections if available
#
# @examples
# result <- dodoCall(
#    dgraphRequest,
#    postText=q)
#
#
# @export
#
#
# connectToDODO <- function(host = "localhost",
#                           port = 8080,
#                           remember = T,
#                           reconnect = T){
#   #########################@
#   ##
#   dodoDir <- file.path(Sys.getenv("HOME"),"R","DODO")
#   dir.create(dodoDir,
#              showWarnings = F,
#              recursive = T)
#   conFile <- file.path(dodoDir,
#                        "DODO-Connection.rda")
#   connection <- list()
#   if(reconnect){
#     if(file.exists(conFile)){
#       load(conFile)
#       if(length(connection)==0){
#         checkDODOConn()
#         return(FALSE)
#       }else{
#         message("Using last connection")
#         host <- connection[["host"]]
#         port <- connection[["port"]]
#       }
#     }else{
#       connection <- list(
#         host = host,
#         port = port)
#     }
#   }else{
#     connection <- list(
#       host = host,
#       port = port)
#   }
#   #######################@
#   ## Connect to DODO
#   assign("graph",
#          DgraphR::startDgraph(url = sprintf("http://%s:%s",host,port)),
#          envir = dodoEnv)
#   if(!exists("graph", dodoEnv)){
#     warning("You should check the connection or availability of the DODO database")
#   }else{
#     message("A connection to DODO dgraph host ",
#             sprintf("http://%s:%s",
#                     host,
#                     port),
#             " was established",
#             appendLF = T)
#   }
#   ######################@
#   ## Remember connection
#   if(remember){
#     save(connection, file = conFile)
#   }
#   invisible(TRUE)
# }

###############################################################################@
#' Connect to a neo4j DODO database
#'
#' @param url a character string. The host and the port are sufficient
#' (e.g: "localhost:7474")
#' @param username a character string (if NA ==> no auth)
#' @param password a character string (if NA ==> no auth)
#' @param connection the id of the connection already registered to use. By
#' default the first registered connection is used.
#' @param remember if TRUE the connection is registered. All the registered
#' connections can be listed with [ls_dodo_connections] and any of
#' them can be forgotten with [forget_dodo_connection].
#' @param importPath the path to the import folder for loading information
#' in DODO (used only when feeding the database ==> default: NA)
#'
#' @return This function does not return any value. It prepares the DODO
#' environment to allow transparent DB calls.
#'
#' @details Be carefull that you should reconnect to DODO database each time
#' the environment is reloaded.
#'
#' @seealso [check_dodo_connection], [ls_dodo_connections],
#' [forget_dodo_connection]
#'
#' @export
#'
connectToDODO <- function(
  url=NULL, username=NULL, password=NULL, connection=1,
  remember=TRUE,
  importPath=NA
){
  dodoDIR <- file.path(
    Sys.getenv("HOME"), "R", "DODO"
  )
  dir.create(dodoDIR, showWarnings=FALSE, recursive=TRUE)
  conFile <- file.path(
    dodoDIR, "DODO-Connections.rda"
  )
  connections <- list()
  if(file.exists(conFile)){
    load(conFile)
  }
  if(length(url)==0 && length(username)==0 && length(password)==0){
    if(length(connections)==0){
      check_dodo_connection()
      return(FALSE)
    }else{
      url <- connections[[connection]]["url"]
      username <- connections[[connection]]["username"]
      password <- connections[[connection]]["password"]
    }
    connections <- c(connections[connection], connections[-connection])
  }else{
    if(length(username)==0 && length(password)==0){
      username <- password <- NA
    }
    connections <- c(
      list(c(url=url, username=username, password=password)),
      connections
    )
  }
  ## The graph DB
  try(assign(
    "graph",
    neo2R::startGraph(
      url=url,
      username=username,
      password=password,
      importPath=importPath
    ),
    dodoEnv
  ))
  corrConn <- check_dodo_connection(verbose=TRUE)
  if(!corrConn){
    rm("graph", envir=dodoEnv)
    return(FALSE)
  }else{
    connections[[1]][colnames(attr(corrConn, "dbVersion")[1,])] <-
      as.character(attr(check_dodo_connection(), "dbVersion")[1,])
  }
  ##
  if(remember){
    connections <- connections[which(
      !duplicated(unlist(lapply(
        connections,
        function(x){
          x["url"]
        }
      )))
    )]
    save(connections, file=conFile)
  }
  ## The cache directory
  cachedbDir <- file.path(
    Sys.getenv("HOME"), "R",
    "DODO",
    paste(
      sub(
        "[:]", "..",
        sub(
          "[/].*$", "",
          sub("^https{0,1}[:][/]{2}", "", url)
        )
      ),
      username,
      sep=".."
    )
  )
  dir.create(cachedbDir, showWarnings=FALSE, recursive=TRUE)
  cachedbFile <- file.path(cachedbDir, "0000-DODO-cache.rda")
  assign(
    "cachedbFile",
    cachedbFile,
    dodoEnv
  )
  if(file.exists(cachedbFile)){
    load(cachedbFile)
  }else{
    cache <- data.frame(
      name=character(),
      file=character(),
      stringsAsFactors=FALSE
    )
  }
  assign(
    "cache",
    cache,
    dodoEnv
  )
  ## Managing cache vs DB version
  check_dodo_cache(newCon=TRUE)
}

###############################################################################@
#' Check if there is a connection to a DODO database
#'
#' @param verbose if TRUE print information about the DODO connection
#' (default: FALSE).
#'
#' @return
#'  - TRUE if the connection can be established
#'  - Or FALSE if the connection cannot be established or the "System" node
#'  does not exist or does not have "DODO" as name or any version recorded.
#'
#' @seealso [connect_to_dodo]
#'
#' @export
#'
checkDODOConn <- function(verbose=FALSE){
  if(!exists("graph", dodoEnv)){
    warning(
      "You should connect to a DODO DB using the connect_to_dodo function"
    )
    return(FALSE)
  }
  if(verbose) message(get("graph", dodoEnv)$url)
  dbVersion <- try(call_dodo(
    f=neo2R::cypher,
    query=neo2R::prepCql(c(
      'MATCH (n:System) RETURN',
      'n.name as name, n.instance as instance, n.version as version'
    )),
    dodoCheck=FALSE
  ))
  if(inherits(dbVersion, "try-error")){
    return(FALSE)
  }
  if(is.null(dbVersion)){
    dbSize <- call_dodo(
      f=neo2R::cypher,
      query='MATCH (n) WITH n LIMIT 1 RETURN count(n);',
      dodoCheck=FALSE
    )[,1]
    if(is.null(dbSize)){
      warning("No connection")
      return(FALSE)
    }
    if(dbSize==0){
      warning("DODO DB is empty !")
      return(TRUE)
    }else{
      warning("DB is not empty but without any System node. Check url.")
      return(FALSE)
    }
  }
  if(verbose){
    message(dbVersion$name)
    message(dbVersion$instance)
    message(dbVersion$version)
  }
  if(
    is.null(dbVersion$name) || dbVersion$name!="DODO" ||
    is.null(dbVersion$instance) ||
    is.null(dbVersion$version)
  ){
    warning("Wrong system. Check url.")
    print(get("graph", dodoEnv)$url)
    return(FALSE)
  }
  toRet <- TRUE
  attr(toRet, "dbVersion") <- dbVersion
  return(toRet)
}

###############################################################################@
#' List all registered DODO connection
#'
#' @seealso [connect_to_dodo],
#' [forget_dodo_connection], [check_dodo_connection]
#'
#' @export
#'
listConnections <- function(){
  conFile <- file.path(
    Sys.getenv("HOME"), "R", "DODO", "DODO-Connections.rda"
  )
  connections <- list()
  if(file.exists(conFile)){
    load(conFile)
  }
  return(connections)
}

###############################################################################@
#' Forget a DODO connection
#'
#' @param connection the id of the connection to forget.
#'
#' @seealso [ls_dodo_connections],
#' [connect_to_dodo], [check_dodo_connection]
#'
#' @export
#'
forget_dodo_connection <- function(connection){
  conFile <- file.path(
    Sys.getenv("HOME"), "R", "DODO", "DODO-Connections.rda"
  )
  connections <- list()
  if(file.exists(conFile)){
    load(conFile)
  }
  connections <- connections[-connection]
  save(connections, file=conFile)
}


#'========================================================================================
#'========================================================================================
#' Get concept description
#' 
#' Describing provided concept identifiers (when available) using disease label
#' 
#' @param ids a character vector of  concept identifier to search (e.g. "MONDO:0005027")
#' @return vector of disease labels
#' 
#' @export
describeConcept <- function(ids){
  ## Checking
  if(is.null(ids)){ 
    stop('Please provide ID')
  }
  
  cql <- c('MATCH (n)',
           'WHERE n.name IN $from',
           'RETURN n.name as id, n.label as label')
  toRet <- call_dodo(
    neo2R::cypher,
    prepCql(cql),
    parameters = list(from = as.list(ids)),
    result = "row") %>%
    tibble::as_tibble()
  return(toRet)
}

#'========================================================================================
#'========================================================================================
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
dodoCall <- function(f, ..., dodoCheck=FALSE){
  if(dodoCheck) if(!check_dodo_connection()){
    stop("No connection")
  }
  do.call(f, list(graph=get("graph", dodoEnv), ...))
}


#'========================================================================================
#'========================================================================================
#' List of databases in DODO
#' 
#' lists all databases present in the database
#' 
#' @examples listDB()
#'
#' @return Returns a list of all database in DODO
#'
#' @export
#'
listDB <- function(){
  ## Prepare query
  cql <- c('MATCH (n:Database)<-[r:is_in]-(d)',
           'RETURN n.name as database, count(r) as count')
  toRet <- call_dodo(
    neo2R::cypher,
    prepCql(cql),
    result = "row"
  ) %>%
    tibble::as_tibble()
  return(toRet)
}

#'========================================================================================
#'========================================================================================
#' List of nodes types in DODO
#' 
#' Lists the different type of node present in the database
#' 
#' @examples listType()
#'
#' @return Returns a list of all database in DODO
#'
#' @export
#'
listType <- function(){
  cql <- c('MATCH (n)',
           'RETURN labels(n) as type, count(n) as count')
  toRet <- call_dodo(
    neo2R::cypher,
    prepCql(cql),
    result = "row"
  ) %>%
    tibble::as_tibble()
  return(toRet)
}

#'========================================================================================
#'========================================================================================
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
#' getConceptUrl(ids = "MONDO:0005027")
#' 
#' @export
#'
getConceptUrl <- function(ids,
                          exact = TRUE){
  ## Checking
  if(is.null(ids)){ 
    stop('Please provide ID')
  }
  
  cql <- c('MATCH (n)-[r:is_in]->(db:Database)',
           'WHERE n.name IN $from',
           'RETURN n.name as id, db.name as origin, db.idURL as url')
  toRet <- call_dodo(
    neo2R::cypher,
    prepCql(cql),
    parameters = list(from = as.list(ids)),
    result = "row") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(db = gsub(":.*", "", id),
                  shortID = gsub(".*:", "", id),
                  url = sprintf(stringr::str_replace(url, "%3A", "%%3A"), shortID))
  
  if(exact){
    toRet <- dplyr::filter(toRet,
                           db == origin)
  }  
  return(toRet)
}


#'========================================================================================
#'========================================================================================
#' Returns current version
#' 
#' Returns the version of the current DODO database
#'
#' @examples getVersion()
#'
#' @return Returns current version of DODO instance
#'
#' @export
#'
getVersion <- function(){
  cql <- c('MATCH (f {name: "DODO"})',
           'RETURN f.name as Name, f.instance as Instance, f.version as Version')
  toRet <- call_dodo(
    neo2R::cypher,
    prepCql(cql),
    result = "row"
  ) %>%
    tibble::as_tibble()
  return(toRet)
}

#'=======================================================================================
#'=======================================================================================
#' Show the data model of DODO
#'
#' Show the shema of the DODO data model.
#'
#' @export
#'
getSchema <- function(){
  pkgname <- utils::packageName()
  htmlFile <- system.file(
    "documentation", "data-model", "DODO.html",
    package=pkgname
  )
  utils::browseURL(paste0('file://', htmlFile))
}

###############################################################################@
#' Feeding DODO: Imports a data.frame in the DODO graph database
#'
#' Not exported to avoid unintended modifications of the DB.
#'
#' @param cql the CQL query to be applied on each row of toImport
#' @param toImport the data.frame to be imported as "row".
#' Use "row.FIELD" in the cql query to refer to one FIELD of the toImport
#' data.frame
#' @param ... additional parameters to the [neo2R::import_from_df()] function.
#'
#' @return the results of the query
#'
#' @seealso [call_dodo()]
#'
#' @importFrom utils write.table
#'
importinDODO <- function(
  cql, toImport, ...
){
  neo2R::import_from_df(graph=get("graph", dodoEnv), cql=cql, toImport, ...)
}


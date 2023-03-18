#========================================================================================@
#========================================================================================@
#' Connect to a neo4j DODO database
#'
#' @param url a character string. The host and the port are sufficient
#' (e.g: "localhost:7474")
#' @param username a character string (if NA ==> no auth)
#' @param password a character string (if NA ==> no auth)
#' @param connection the id of the connection already registered to use. By
#' default the first registered connection is used.
#' @param remember if TRUE the connection is registered. All the registered
#' connections can be listed with [list_dodo_connections] and any of
#' them can be forgotten with [forget_dodo_connection].
#' @param importPath the path to the import folder for loading information
#' in DODO (used only when feeding the database ==> default: NA)
#' @param .opts a named list identifying the curl
#' options for the handle (see [neo2R::startGraph()]).
#'
#' @return This function does not return any value. It prepares the DODO
#' environment to allow transparent DB calls.
#'
#' @details Be carefull that you should reconnect to DODO database each time
#' the environment is reloaded.
#'
#' @seealso [check_dodo_connection], [list_dodo_connections],
#' [forget_dodo_connection]
#'
#' @export
#'
connect_to_dodo <- function(
  url=NULL, username=NULL, password=NULL, connection=1,
  remember=FALSE,
  importPath=NA,
  .opts=list()
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
      importPath=importPath,
      .opts=.opts
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

#========================================================================================@
#========================================================================================@
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
check_dodo_connection <- function(verbose=FALSE){
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
  dbVersion$url <- get("graph", dodoEnv)$url
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

#========================================================================================@
#========================================================================================@
#' List all registered DODO connection
#'
#' @seealso [connect_to_dodo],
#' [forget_dodo_connection], [check_dodo_connection]
#'
#' @export
#'
list_dodo_connections <- function(){
  conFile <- file.path(
    Sys.getenv("HOME"), "R", "DODO", "DODO-Connections.rda"
  )
  connections <- list()
  if(file.exists(conFile)){
    load(conFile)
  }
  return(connections)
}

#========================================================================================@
#========================================================================================@
#' Forget a DODO connection
#'
#' @param connection the id of the connection to forget.
#'
#' @seealso [list_dodo_connections],
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
dodoCall <- function(f, ..., dodoCheck=FALSE){
  if(dodoCheck) if(!check_dodo_connection()){
    stop("No connection")
  }
  do.call(f, list(graph=get("graph", dodoEnv), ...))
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
#'
list_database <- function(){
  ## Prepare query
  cql <- c('MATCH (n:Database)<-[r:is_in]-(d)',
           'RETURN n.name as database, count(r) as count')
  toRet <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    result = "row"
  ) %>%
    tibble::as_tibble()
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
#'
list_node_type <- function(){
  cql <- c('MATCH (n)',
           'RETURN labels(n) as type, count(n) as count')
  toRet <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    result = "row"
  ) %>%
    tibble::as_tibble()
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
describe_concept <- function(ids, verbose = FALSE){
  ## Checking
  if(is.null(ids)){ 
    stop('Please provide ID')
  }
  
  cql <- c('MATCH (n)',
           'WHERE n.name IN $from',
           'RETURN n.name as id, n.label as label, n.level as level, n.definition as definition')
  if(verbose){
    cat(cql, sep = "\n")
  }
  
  toRet <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    parameters = list(from = as.list(ids)),
    result = "row") %>%
    tibble::as_tibble()
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
#'
get_concept_url <- function(ids,
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
    neo2R::prepCql(cql),
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
#'
get_version <- function(){
  cql <- c('MATCH (f {name: "DODO"})',
           'RETURN f.name as Name, f.instance as Instance, f.version as Version')
  toRet <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    result = "row"
  ) %>%
    tibble::as_tibble()
  return(toRet)
}

#========================================================================================@
#========================================================================================@
#' Show the data model of DODO
#'
#' Show the shema of the DODO data model.
#'
#' @export
#'
show_dodo_model <- function(){
  pkgname <- utils::packageName()
  htmlFile <- system.file(
    "documentation", "data-model", "DODO.html",
    package=pkgname
  )
  utils::browseURL(paste0('file://', htmlFile))
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


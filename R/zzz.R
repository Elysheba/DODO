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
connect_to_dodo <- function(
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
ls_dodo_connections <- function(){
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

###############################################################################@
dodoEnv <- new.env(hash=TRUE, parent=emptyenv())
.onLoad <- function(libname, pkgname){
   connect_to_dodo()
}

###############################################################################@
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
   do.call(f, list(graph=get("graph", dodoEnv), ...))
}


###############################################################################@
#' Cached neo4j call
#'
#' This function calls neo4j DB the first time a query is sent and puts
#' the result in the cache file. The next time the same query is
#' called, it loads the results directly from the cache file.
#'
#' Use only with "row" result returned by DB request.
#'
#' Internal use.
#'
#' @param ... params for [call_dodo()]
#' @param tn the name of the cached table
#' @param recache boolean indicating if the CQL query should be run even if
#' the table is already in cache
#'
#' @return The results of the [call_dodo()].
#'
#' @seealso [cache_dodo_results()], [load_dodo_results()], [call_dodo()]
#'
call_dodo_cache <- function(
   ...,
   tn,
   recache=FALSE
){
   cache <- check_dodo_cache()
   if(tn %in% rownames(cache) & !recache){
      toRet <- load_dodo_results(tn)
   }else{
      toRet <- call_dodo(...)
      cache_dodo_results(value=toRet, name=tn)
   }
   return(toRet)
}

###############################################################################@
#' Put a DODO query result in cache
#'
#' Internal use
#'
#' @param value the result to cache
#' @param name the name of the query
#'
#' @seealso [call_dodo_cache()], [load_dodo_results()]
#'
cache_dodo_results <- function(
   value,
   name
){
   if(length(grep("^0000-", name))>0){
      stop('names starting by "0000-" are reserved')
   }
   cache <- get("cache", dodoEnv)
   cachedbFile <- get("cachedbFile", dodoEnv)
   cachedbDir <- dirname(cachedbFile)
   file <- paste0(name, ".rda")
   save(value, file=file.path(cachedbDir, file))
   cache[name, ] <- data.frame(name=name, file=file, stringsAsFactors=FALSE)
   save(cache, file=cachedbFile)
   assign(
      "cache",
      cache,
      dodoEnv
   )
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
import_in_dodo <- function(
   cql, toImport, ...
){
   neo2R::import_from_df(graph=get("graph", dodoEnv), cql=cql, toImport, ...)
}


###############################################################################@
#' Get a DODO query result from cache
#'
#' Internal use
#'
#' @param name the name of the query
#'
#' @seealso [call_dodo_cache()], [cache_dodo_results()]
#'
load_dodo_results <- function(name){
   value <- NULL
   cache <- get("cache", dodoEnv)
   cachedbFile <- get("cachedbFile", dodoEnv)
   cachedbDir <- dirname(cachedbFile)
   if(!name %in% rownames(cache)){
      stop(sprintf("%s not in cache", name))
   }
   load(file.path(cachedbDir, cache[name, "file"]))
   return(value)
}

###############################################################################@
#' Check DODO cache
#'
#' This function checks information recorded into DODO cache
#' and resets it if not relevant.
#'
#' Internal use.
#'
#' @param newCon if TRUE for the loading of the system information file
#'
#' @seealso [clear_dodo_cache], [ls_dodo_cache]
#'
check_dodo_cache <- function(newCon=FALSE){
   if(!check_dodo_connection()){
      stop("Not connected to DODO")
   }
   dbSize <- call_dodo(
      neo2R::cypher,
      'MATCH (n) WITH n LIMIT 1 RETURN count(n);'
   )[,1]
   if(dbSize==0){
      warning("Clearing cache")
      cache <- clear_dodo_cache()
   }else{
      dbVersion <- call_dodo(
         neo2R::cypher,
         query=neo2R::prepCql(c(
            'MATCH (n:System) RETURN',
            'n.name as name, n.instance as instance, n.version as version'
         ))
      )
      dbVersion$rdodo <- utils::packageDescription(utils::packageName())$Version
      ##
      cachedbFile <- get("cachedbFile", dodoEnv)
      cachedbDir <- dirname(cachedbFile)
      sysFile <- file.path(cachedbDir, "0000-DODO-system.rda")
      if(file.exists(sysFile)){
         if(!exists("system", envir=dodoEnv) | newCon){
            load(file.path(cachedbDir, "0000-DODO-system.rda"), envir=dodoEnv)
         }
         system <- get("system", envir=dodoEnv)
         if(
            any(!names(dbVersion) %in% names(system)) ||
            dbVersion$name != system$name ||
            dbVersion$instance != system$instance ||
            dbVersion$version != system$version ||
            dbVersion$rdodo != system$rdodo
         ){
            message("Inconsitent cache == > clearing cache")
            cache <- clear_dodo_cache()
            system <- dbVersion
            save(system, file=sysFile)
            assign(
               "system",
               system,
               dodoEnv
            )
         }else{
            cache <- get("cache", dodoEnv)
         }
      }else{
         message("No recorded version ==> clearing cache")
         cache <- clear_dodo_cache()
         system <- dbVersion
         save(system, file=sysFile)
         assign(
            "system",
            system,
            dodoEnv
         )
      }
   }
   invisible(cache)
}

###############################################################################@
#' Clear the DODO cache SQLite database
#'
#' @param queries a character vector of the names of queries to remove.
#' If NULL all queries are removed.
#' @param force if TRUE clear the DODO cache table even if cache
#' file is not found
#' @param hard if TRUE remove everything in cache without checking file names
#' @param verbose display some information during the process
#'
#' @seealso [ls_dodo_cache]
#'
#' @export
#'
clear_dodo_cache <- function(
   queries=NULL, force=FALSE, hard=FALSE, verbose=FALSE
){
   if(!check_dodo_connection(verbose=FALSE)){
      stop("Unsuccessful connection")
   }
   if(hard){
      cachedbFile <- get("cachedbFile", dodoEnv)
      cachedbDir <- dirname(cachedbFile)
      file.remove(list.files(path=cachedbDir, full.names=TRUE))
      cache <- data.frame(
         name=character(),
         file=character(),
         stringsAsFactors=FALSE
      )
      assign(
         "cache",
         cache,
         dodoEnv
      )
      check_dodo_cache()
      invisible()
   }
   cache <- get("cache", dodoEnv)
   cachedbFile <- get("cachedbFile", dodoEnv)
   cachedbDir <- dirname(cachedbFile)
   if(is.null(queries)){
      queries <- cache
   }else{
      if(any(!queries %in% rownames(cache))){
         warning(sprintf(
            "%s not in cache",
            paste(setdiff(queries, rownames(cache)), collapse=", ")
         ))
      }
      queries <- cache[intersect(queries, rownames(cache)),]
   }
   for(tn in rownames(queries)){
      if(verbose){
         message(paste("Removing", tn, "from cache"))
      }
      if(file.remove(file.path(cachedbDir, queries[tn, "file"]))){
         cache <- cache[setdiff(rownames(cache), tn),]
         save(cache, file=cachedbFile)
         assign(
            "cache",
            cache,
            dodoEnv
         )
      }else{
         if(!force){
            stop(paste0(
               "Could not remove the following file: ",
               file.path(cachedbDir, queries[tn, "file"]), "\n",
               "Check cache files and/or clear the whole cache using force=TRUE"
            ))
         }else{
            warning(paste0(
               "Could not remove the following file: ",
               file.path(cachedbDir, queries[tn, "file"]), "\n",
               "Clearing cache table anyway (force=TRUE)"
            ))
            cache <- cache[setdiff(rownames(cache), tn),]
            save(cache, file=cachedbFile)
            assign(
               "cache",
               cache,
               dodoEnv
            )
         }
      }
   }
   invisible(cache)
}

###############################################################################@
#' List all the DODO queries in cache and the total size of the cache
#'
#' @param verbose if TRUE (default) prints a message displaying the total
#' size of the cache
#'
#' @return A data.frame giving for each query (row names) its size in Bytes
#' (column "size") and in human readable format (column "hr"). The
#' attribute "Total" corresponds to the sum of all the file size.
#'
#' @seealso [clear_dodo_cache]
#'
#' @export
#'
ls_dodo_cache <- function(verbose=TRUE){
   ##
   sunits <- c("B", "KB", "MB", "GB", "TB")
   ##
   if(!check_dodo_connection()){
      stop("Unsuccessful connection")
   }
   cache <- get("cache", dodoEnv)
   if(nrow(cache)==0){
      message("Empty cache")
      total <- data.frame(
         size=0,
         hr="0 B",
         stringsAsFactors=FALSE
      )
      rownames(total) <- "Total"
      toRet <- data.frame(
         size=numeric(),
         hr=character(),
         stringsAsFactors=FALSE
      )
      attr(toRet, "Total") <- total
      return(toRet)
   }
   cachedbDir <- dirname(get("cachedbFile", dodoEnv))
   toRet <- file.size(file.path(cachedbDir, cache$file))
   names(toRet) <- cache$name
   total <- sum(toRet)
   toRetUnits <- log2(toRet)%/%10
   toRetHR <- lapply(
      1:length(toRet),
      function(i){
         format(
            toRet[i]/(2^(10*toRetUnits[i])),
            digit=1,
            nsmall=ifelse(toRetUnits[i]==0, 0, 1)
         )
      }
   )
   toRet <- data.frame(
      size=toRet,
      hr=paste(toRetHR, sunits[toRetUnits+1]),
      stringsAsFactors=FALSE
   )
   totalUnits <- log2(total)%/%10
   if(is.na(totalUnits)){
      totalUnits <- 0
   }
   totalHR <- format(
      total/(2^(10*totalUnits)),
      digit=1,
      nsmall=ifelse(totalUnits==0, 0, 1)
   )
   total <- data.frame(
      size=total,
      hr=paste(totalHR, sunits[totalUnits+1]),
      stringsAsFactors=FALSE
   )
   rownames(total) <- "Total"
   attr(toRet, "Total") <- total
   if(verbose){
      message(paste("Total cache size on disk:", total$hr))
   }
   return(toRet[order(toRet$size, decreasing=TRUE),])
}

###############################################################################@
#' Build a disease network
#' 
#' Building a network of disease identifiers based on their relationships
#' described in different database integrated in DODO using two helper functions 
#' *buildDisNetByTerm* and *buildDisNetByID*. 
#' 
#' 
#' @param ids: disease ids to include in the network
#' @param seed: special disease ids considered as the network seed (all of them should be in ids)
#' @param forwardAmbiguity level of forward ambiguity allowed
#' (default: 10000 ==> ~no filter)
#' @param backwardAmbiguity level of backward ambiguity allowed
#' (default: 1 ==> no ambiguity allowed)
#' @param avoidOrigin: allows to avoid traversing parent/child edges from a particular ontology
#' @param verbose show query input (default = FALSE)
#' 
#' @details 
#' The disNet object is constructed around the return 
#' query results also providing information on their relationships. Therefore the *buildDisNet* function
#' doesn't deal with transitivity of cross-reference edges, so the forward and backward ambiguity
#' are applied for both type of cross-references (*is_xref* and *is_related*) consistently.
#' The default implementation only has a limit on the backward ambiguity (see vignette).
#' In addition, it is possible to avoid returning a particular hierarchical relationship 
#' by specifiying *avoidOrigin*.
#' 
#' @return A disease network is a list with the following elements
#' \itemize{
#' \item nodes: a data.frame describind disease nodes with the following columns
#'    \itemize{
#'    \item id (character): disease ids (database:shortID)
#'    \item database (character): disease databases
#'    \item shortID (character): disease short identifiers
#'    \item label (character): disease labels
#'    \item definition (character): disease descriptions
#'    \item level (integer): maximum level the identifier holds in the hierarchical ontology tree
#'    \item type (character): type of node (disease or phenotype)
#'    }
#' \item synonyms: a data.frame with disease synonyms with the following columns
#'    \itemize{
#'    \item id (character): disease ids
#'    \item synonym (character): disease synonyms
#'    }
#' \item children: a data.frame with ontology information
#'    \itemize{
#'    \item parent (character): parent disease ids
#'    \item child (character): child disease ids
#'    \item origin (character): ontology of origin where the parent/child relationship is recorded
#'    }
#' \item xref: a data.frame with cross-references
#'    \itemize{
#'    \item from (character): disease 1 ids
#'    \item to (character): disease 2 ids
#'    \item ur (character): xref
#'    identifier: \code{paste(sort(c(xref$from, xref$to)), collapse="<-->")}
#'    \item forwardAmbiguity (numeric): number of cross-references
#'    between disease 1 and database 2
#'    \item backwardAmbiguity (numeric): number of cross-references
#'    between disease 2 and database 1
#'    }
#' \item pheno: a data.frame with phenotype information
#'    \itemize{
#'    \item disease (character): disease identifier
#'    \item phenotype (character): phenotype identifier
#'    }
#' \item seed: a character vector of disease ids
#' }
#' The following statements are ensured in normalized disease networks:
#' \itemize{
#'    \item All seed are in nodes$id
#'    \item All synonyms$id are in nodes$id
#'    \item All children$parent are in nodes$id
#'    \item All children$child are in nodes$id
#'    \item All xref$from are in nodes$id
#'    \item All xref$to are in nodes$id
#'    \item xref$ur are not duplicated
#' }
#' 
#' @examples 
#' x <- extDisNet$nodes$id
#' buildDisNet(ids = x$kept, seed = x$kept)
#' 
#' @seealso buildDisNetByID, buildDisNetByTerm
#' 
#' @export
#' 
buildDisNet <- function(ids = c(), 
                        seed = c(), 
                        backwardAmbiguity = 1, 
                        forwardAmbiguity = NULL, 
                        avoidOrigin = NULL){
  if(any(!seed %in% ids)){
    stop("All seed should be in ids")
  }
  ## avoidOrigin 
  if(!is.null(avoidOrigin)){
    match.arg(avoidOrigin, listDB()$db, several.ok = T)
  }
  
  stopifnot(is.null(forwardAmbiguity) || forwardAmbiguity == 1,
            is.null(backwardAmbiguity) || backwardAmbiguity == 1)
  
  if(is.null(backwardAmbiguity)){
    relationship <- "is_related|is_xref"
  }else{
    relationship <- "is_related_nba|is_xref_nba"
  }
  
  ## Empty disNet
  diseaseNetwork <- list(
    nodes = tibble::tibble(
      id = character(),
      database = character(),
      shortID = character(),
      label = character(),
      definition = character(),
      level = integer(),
      type = character()
    ),
    synonyms = tibble::tibble(
      id = character(),
      synonym = character()
    ),
    children = tibble::tibble(
      parent = character(),
      child = character(),
      origin = character()
    ),
    xref = tibble::tibble(
      from = character(),
      to = character(),
      forwardAmbiguity = numeric(),
      backwardAmbiguity = numeric(),
      ur = character() 
    ),
    pheno = tibble::tibble(
      disease = character(),
      phenotype = character()
    ),
    seed=character()
  )
  if(length(ids) > 0){
    ## nodes
    cql <- c('MATCH (n)',
             'WHERE n.name IN $from',
             'RETURN DISTINCT n.name as id, n.label as label, n.definition as definition, ',
             'n.shortID as shortID, n.level as level, labels(n) as type')
    nodes <- call_dodo(neo2R::cypher,
                       prepCql(cql),
                       parameters = list(from = as.list(ids)),
                       result = "row") %>%
      tibble::as_tibble() 
    if(nrow(nodes) == 0){
      nodes <- diseaseNetwork$nodes
    }else{
      nodes <- nodes %>%
        dplyr::mutate(database = gsub(":.*", "", id))
    }
    
    ## synonyms
    cql <- c('MATCH (n)-[r:is_known_as]-(s:Synonym)',
             'WHERE n.name IN $from',
             'RETURN DISTINCT n.name as id, s.value as synonym')
    synonyms <- call_dodo(neo2R::cypher,
                       prepCql(cql),
                       parameters = list(from = as.list(ids))) %>%
      tibble::as_tibble()
    if(nrow(synonyms) == 0){
      synonyms <- diseaseNetwork$synonyms
    }
             
    ## cross-references
    cql <- c(
      sprintf("MATCH (n)-[r:%s]-(x)",
              relationship),
             "WHERE n.name IN $from AND x.name IN $from",
             "RETURN DISTINCT n.name as from, x.name as to,",
             "r.FA as forwardAmbiguity, r.BA as backwardAmbiguity")
    xref <- call_dodo(neo2R::cypher,
                       prepCql(cql),
                       parameters = list(from = as.list(ids)),
                       result = "row") %>%
      tibble::as_tibble()
    if(nrow(xref) == 0){
      xref <- diseaseNetwork$xref
    }else{
      xref <- xref  %>%
        dplyr::mutate(ur = paste(pmin(xref$from, xref$to), pmax(xref$from, xref$to), sep = "<->")) %>%
        dplyr::distinct(ur, .keep_all = TRUE)
    }
  
  ## hierarchy
  cql <- c('MATCH (c)-[r:is_a]-(p)',
           sprintf('WHERE %s c.name IN $from AND p.name IN $from',
                   ifelse(is.null(avoidOrigin),
                          "",
                          "r.origin IN $origin AND")),
           'RETURN DISTINCT p.name as parent, c.name as child, r.origin as origin')
  children <- call_dodo(neo2R::cypher,
                    prepCql(cql),
                    parameters = list(from = as.list(ids),
                                      origin = as.list(avoidOrigin)),
                    result = "row") %>%
    tibble::as_tibble()
  if(nrow(children) == 0){
    children <- diseaseNetwork$children
  }
  
  ## phenotypes
  cql <- c('MATCH (n)-[r:has_pheno]-(p)',
           'WHERE n.name IN $from AND p.name IN $from',
           'RETURN DISTINCT n.name as disease, p.name as phenotype')
  pheno <- call_dodo(neo2R::cypher,
                    prepCql(cql),
                    parameters = list(from = as.list(ids)),
                    result = "row") %>%
    tibble::as_tibble()
  if(nrow(pheno) == 0){
    pheno <- diseaseNetwork$pheno
  }
  
  ## seed
  seed <- seed

  ## Reset rownames
  rownames(nodes) <- NULL
  rownames(synonyms) <- NULL
  rownames(children) <- NULL
  rownames(xref) <- NULL
  rownames(pheno) <- NULL
  ##
  diseaseNetwork <- list(nodes = nodes,
                         synonyms = synonyms,
                         children = children,
                         xref = xref,
                         pheno = pheno,
                         seed = seed)
  }
  diseaseNetwork <- structure(diseaseNetwork,
                              class = "disNet")
  return(diseaseNetwork)
}

###############################################################################@
#' Build a network of diseases according to provided identifiers
#' 
#' Querying for (a) disease identifier(s) is simplified using the 
#' helper function *buildDisNetByID* using a vector or dataframe as input.
#' 
#' 
#' @param ids a character vector of identifier to search formatted as *DB:id* (e.g. "MONDO:0005027")
#' @param forwardAmbiguity level of forward ambiguity allowed
#' (default: 10000 ==> ~no filter)
#' @param backwardAmbiguity level of backward ambiguity allowed
#' (default: 1 ==> no ambiguity allowed)
#' @param avoidOrigin: allows to avoid traversing parent/child edges from a particular ontology (default = NULL)
#' 
#' @details 
#' 
#' The disNet object is constructed around the return 
#' query results also providing information on their relationships. Therefore the *buildDisNet* function
#' doesn't deal with transitivity of cross-reference edges, so the forward and backward ambiguity
#' are applied for both type of cross-references (*is_xref* and *is_related*) consistently.
#' The default implementation only has a limit on the backward ambiguity (see vignette).
#' In addition, it is possible to avoid returning a particular 
#' hierarchical relationship by specifiying *avoidOrigin*.
#' 
#' @return A normalized disease network 
#' 
#' @examples 
#' 
#' buildDisNetByID(ids = c("ORPHA:86814","MONDO:0005099"))
#' 
#' @seealso buildDisNet, buildDisNetByTerm
#' 
#' @export
#' 
buildDisNetByID <- function(ids,
                            backwardAmbiguity = 1, 
                            forwardAmbiguity = NULL, 
                            avoidOrigin = NULL
){
  seed <- findID(ids = ids)
  return(buildDisNet(ids = seed, 
                     seed = seed, 
                     backwardAmbiguity = backwardAmbiguity, 
                     forwardAmbiguity = forwardAmbiguity, 
                     avoidOrigin = avoidOrigin))
}


###############################################################################@
#' Build a network of diseases according to provided identifiers
#' 
#' Querying for (a) disease identifier(s) is simplified using the 
#' helper function *buildDisNetByID* using a vector or dataframe as input.
#' 
#' 
#' @param term a character vector of identifier to search formatted as *DB:id* (e.g. "MONDO:0005027")
#' @param fields the field(s) where to look for matches.
#' "synonym", "label", "definition" are supported and all of them are considered
#' by default
#' @param forwardAmbiguity level of forward ambiguity allowed
#' (default: 10000 ==> ~no filter)
#' @param backwardAmbiguity level of backward ambiguity allowed
#' (default: 1 ==> no ambiguity allowed)
#' @param avoidOrigin: allows to avoid traversing parent/child edges from a particular ontology (default = NULL)
#' 
#' @details 
#' 
#' Querying for a search term is simplified using the 
#' helper function *buildDisNetByTerm* where you can specify which type of 
#' information related to a disease you want to query, such as disease label, disease synonyms, 
#' or disease definition, or a combination. The disNet object is constructed around the return 
#' query results also providing information on their relationships. Therefore the *buildDisNet* function
#' doesn't deal with transitivity of cross-reference edges, so the forward and backward ambiguity
#' are applied for both type of cross-references (*is_xref* and *is_related*) consistently.
#' The default implementation only has a limit on the backward ambiguity (see vignette).
#' In addition, it is possible to avoid returning a particular 
#' hierarchical relationship by specifiying *avoidOrigin*.
#' 
#' @return A normalized disease network 
#' 
#' @examples 
#' 
#' buildDisNetByTerm("epilep", fields = c("synonym"))
#' 
#' @seealso buildDisNet, buildDisNetByTerm
#' 
#' @export
#' 
buildDisNetByTerm <- function(term,
                              fields,
                              backwardAmbiguity = 1, 
                              forwardAmbiguity = NULL, 
                              avoidOrigin = NULL
){
  seed <- findTerm(term = term,
                   fields = fields)
  return(buildDisNet(ids = seed, 
                     seed = seed, 
                     backwardAmbiguity = backwardAmbiguity, 
                     forwardAmbiguity = forwardAmbiguity, 
                     avoidOrigin = avoidOrigin))
}

###############################################################################@
#' Check validity 
#' @export

is.disNet <- function(x, ...){
  inherits(x, "disNet")  
}


###############################################################################@
#' Prints a disNet object
#' @export

print.disNet <- function(x, ...){
  
  cat(format(x, ...), sep = "\n")
  
}

###############################################################################@
#' format disNet
format.disNet <- function(x, ...){
  toRet <-    c(capture.output(x$nodes),
                "",
                "The disNet contains:",
                paste(" - ", 
                      x$nodes %>% dplyr::filter(type == "Disease") %>% nrow(), 
                      "disease nodes from", 
                      length(unique(x$nodes %>% filter(type == "Disease") %>% pull(database))), "ontologies and",
                      x$nodes %>% dplyr::filter(type == "Phenotype") %>% nrow(), 
                      "phenotype nodes from", 
                      length(unique(x$nodes %>% filter(type == "Phenotype") %>% pull(database))), "ontologies "),
                paste(" - ", nrow(x$synonyms), "synonyms of the disease nodes"),
                paste(" - ", nrow(x$children), "parent/child edges"),
                paste(" - ", nrow(x$xref), "crossreference edges"),
                paste(" - ", nrow(x$pheno), "phenotype edges"),
                paste(" - ", "The disNet was build based on", length(x$seed), "seeds")
  )
  return(toRet)
}

###############################################################################@
#' Returns the number of nodes in a disNet object
#' @export

length.disNet <- function(x, ...){
  
  nrow(x$nodes)
  
}

###############################################################################@
#' Returns the dimension of the nodes dataframe in a disNet object
#' @export

dim.disNet <- function(x, ...){
  
  dim(x$nodes)
  
}

###############################################################################@
#' Subset `[`
#' @export
#' 
'[.disNet' <- function(x, ...){
  
  stop("Action not allowed on a disNet object")
  
  
}

###############################################################################@
#' Subset `[[`
#' @export
#' 
'[[.disNet' <- function(x, i){
  
  stop("Action not allowed on a disNet object")
  
  
}

###############################################################################@
#' Slice a disNet
#' 
#' Choose rows by their ordinal position in the disNet
#' 
#' @param x disNet object
#' @param n Integer row values. Provide either positive values to keep, or 
#' negative values to drop. The values provided must be either all positive 
#' or all negative. Indices beyond the number of rows in the input are silently ignored.
#' 
#' @export
#' 
slice.disNet <- function(x,
                         n){
  x$nodes <- x$nodes %>%
    dplyr::slice(n)
  return(normalizeDisNet(x))
  
}

###############################################################################@
#' Merge several disease networks
#' 
#' @param ... disease networks to merge
#'
#' @return A normalized merged disease network 
#' 
#' @examples 
#' disNet1 <- buildDisNetByTerm("epilep",fields = c("synonym","label","definition"))
#' disNet2 <- buildDisNetByTerm("alzheim",fields = c("synonym","label","definition"))
#' mergeDisNet <- mergeDisNet(disNet1,disNet2)
#' 
#' @export
c.disNet <- function(...,
                     list = NULL
){
  if(is.null(list)){
    arg <- list(...)
  }else{
    # stopifnot(is.setDisNet(list))
    arg <- list
  }
  disNet <- list(
    nodes = unique(do.call(rbind, lapply(arg, function(x) x$nodes ))),
    synonyms = unique(do.call(rbind, lapply(arg, function(x) x$synonyms))),
    children = unique(do.call(rbind, lapply(arg, function(x) x$children))),
    xref = unique(do.call(rbind, lapply(arg, function(x) x$xref))),
    pheno = unique(do.call(rbind,lapply(arg, function(x) x$pheno))),
    seed = unique(unlist(lapply(arg, function(x) x$seed)))
  )
  disNet <- structure(disNet,
                      class = "disNet")
  return(normalizeDisNet(disNet))
}

###############################################################################@
#' Normalize disease network
#' 
#' This function ensure that disease network properties are fulfilled.
#' 
#' @param disNet a disease network to normalize
#' 
#' @return A normalized disease network, disNet
#' 
#' @export
#' 
normalizeDisNet <- function(disNet){
  if(!is.null(disNet$children)){
    disNet$children <- disNet$children[which(
      disNet$children$parent %in% disNet$nodes$id &
        disNet$children$child %in% disNet$nodes$id
    ),]
  }
  if(!is.null(disNet$xref)){
    disNet$xref <- disNet$xref[which(!duplicated(disNet$xref$ur)),]
    disNet$xref <- disNet$xref[which(
      disNet$xref$from %in% disNet$nodes$id &
        disNet$xref$to %in% disNet$nodes$id
    ),]
  }
  if(!is.null(disNet$pheno)){
    # disNet$pheno <- disNet$pheno[which(!duplicated(disNet$pheno$ur)),]
    disNet$pheno <- disNet$pheno[which(
      disNet$pheno$disease %in% disNet$nodes$id &
        disNet$pheno$phenotype %in% disNet$nodes$id
    ),]
  }
  disNet$synonyms <- disNet$synonyms[which(
    disNet$synonyms$id %in% disNet$nodes$id
  ),]
  disNet$seed <- intersect(disNet$seed, disNet$nodes$id)
  return(disNet)
}


#========================================================================================@
#========================================================================================@
#' Build a disease network
#' 
#' Building a network of disease identifiers based on their relationships
#' described in different database integrated in DODO using two helper functions 
#' *buildDisNetByTerm* and *buildDisNetByID*. 
#' 
#' 
#' @param id disease id to include in the network
#' @param term a character vector of terms to search (e.g. "epilepsy")
#' @param fields the field(s) where to look for matches (default: c(label, synonym)).
#' @param ambiguity level of backward ambiguity allowed
#' (default: 1 ==> no ambiguity allowed)
#' @param avoidOrigin avoid a particular origin 
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
#'    \item id (character): disease id (database:shortID)
#'    \item database (character): disease databases
#'    \item shortID (character): disease short identifiers
#'    \item label (character): disease labels
#'    \item definition (character): disease descriptions
#'    \item level (integer): maximum level the identifier holds in the hierarchical ontology tree
#'    \item type (character): type of node (disease or phenotype)
#'    }
#' \item synonyms: a data.frame with disease synonyms with the following columns
#'    \itemize{
#'    \item id (character): disease id
#'    \item synonym (character): disease synonyms
#'    }
#' \item children: a data.frame with ontology information
#'    \itemize{
#'    \item parent (character): parent disease id
#'    \item child (character): child disease id
#'    \item origin (character): ontology of origin where the parent/child relationship is recorded
#'    }
#' \item xref: a data.frame with cross-references
#'    \itemize{
#'    \item from (character): disease 1 id
#'    \item to (character): disease 2 id
#'    identifier: \code{paste(sort(c(xref$from, xref$to)), collapse="<-->")}
#'    \item forwardAmbiguity (numeric): number of cross-references
#'    between disease 1 and database 2
#'    \item backwardAmbiguity (numeric): number of cross-references
#'    between disease 2 and database 1
#'    \item type (character): type of the cross-reference edge
#'    \item ur (character): unique xref-xref id
#'    }
#' \item alt: a data.frame with alternative identifiers
#'    \itemize{
#'    \item id (character): disease identifier
#'    \item alt (character): alternative disease identifier
#'    }
#' \item pheno: a data.frame with phenotype information
#'    \itemize{
#'    \item disease (character): disease identifier
#'    \item phenotype (character): phenotype identifier
#'    }
#' \item seed: a character vector of disease id
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
#' build_disNet(id = "MONDO:0005027")
#' build_disNet(term = "epilepsy")
#' 
#' @export
#' 
build_disNet <- function(id = NULL, 
                        term = NULL,
                        fields = c("label", "synonym"),
                        ambiguity = NULL, 
                        avoidOrigin = NULL,
                        verbose = FALSE){
  #########################@
  ## Check ----
  fields <- match.arg(fields, c("label", "synonym"), several.ok = TRUE)
  if(!is.null(avoidOrigin)){match.arg(avoidOrigin, 
                                      list_database()$database, 
                                      several.ok = T)}
  
  stopifnot(is.null(ambiguity) || ambiguity == 1,
            xor(is.null(id), is.null(term)))
  
  if(is.null(ambiguity)){
    relationship <- "is_related|is_xref"
  }else{
    relationship <- "is_related_nba|is_xref_nba"
  }
  
  #########################@
  ## Initiate disNet ----
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
      type = character(),
      ur = character() 
    ),
    alt = tibble::tibble(
      id = character(),
      alt = character()
    ),
    pheno = tibble::tibble(
      disease = character(),
      phenotype = character()
    ),
    seed=character()
  )
  
  #########################@
  ## Identifiers input ----
  
  ## Terms
  if(!is.null(term)){
    id <- find_term(term = term,
                   fields = fields)
  }
  
  #########################@
  ## Build query ----
  if(!is.null(id)){
    
    cql.nodes <- c('MATCH (n:Concept)',
                   'WHERE n.name IN $from',
                   'RETURN DISTINCT n.name AS id, n.label AS label, n.definition AS definition, ',
                   'n.shortID AS shortID, n.level AS level, labels(n) AS type')
    cql.syn <- c('MATCH (n:Concept)-[r:is_known_as]-(s:Synonym)',
                 'WHERE n.name IN $from',
                 'RETURN DISTINCT n.name AS id, s.value AS synonym')
    cql.xref <- c(sprintf("MATCH (n:Concept)-[r:%s]->(x:Concept)",
                          relationship),
                  "WHERE n.name IN $from AND x.name IN $from",
                  "RETURN DISTINCT n.name AS from, x.name AS to, ",
                  "r.FA AS forwardAmbiguity, r.BA AS backwardAmbiguity, ",
                  "Type(r) as type")
    cql.child <- c('MATCH (c:Concept)-[r:is_a]->(p:Concept)',
                   sprintf('WHERE %s c.name IN $from AND p.name IN $from',
                           ifelse(is.null(avoidOrigin),
                                  "",
                                  "NOT r.origin IN $origin AND")),
                   'RETURN DISTINCT p.name AS parent, c.name AS child, r.origin AS origin')
    cql.alt <- c('MATCH (c:Concept)<-[r:is_alt]-(a:Concept)',
                 'WHERE c.name in $from AND a.name in $from' ,
                 'RETURN DISTINCT c.name AS id, a.name AS alt')
    cql.pheno <- c('MATCH (n:Disease)-[r:has_pheno]->(p:Phenotype)',
                   'WHERE n.name IN $from AND p.name IN $from',
                   'RETURN DISTINCT n.name AS disease, p.name AS phenotype')
    statements <- list(nodes = neo2R::prepCql(cql.nodes),
                       syn = neo2R::prepCql(cql.syn),
                       xref = neo2R::prepCql(cql.xref),
                       child = neo2R::prepCql(cql.child),
                       pheno = neo2R::prepCql(cql.pheno))
    if(verbose){
      print(statements, sep = "\n")
    }
    
  #######################@
  # Send queries ----
  toRet <- call_dodo(
     neo2R::multicypher,
     queries = statements,
     result = "row",
     parameters = list(from = as.list(id),
                       origin = as.list(avoidOrigin)))
  
  #######################@
  ## Post-processing ----
    
  ## **nodes ----
  nodes <- toRet$nodes %>%
    tibble::as_tibble() 
  if(nrow(nodes) == 0){
    nodes <- diseaseNetwork$nodes
  }else{
    nodes <- nodes %>%
      dplyr::mutate(database = gsub(":.*", "", id))
  }
  
  ## **synonyms ----
  synonyms <- toRet$syn %>%
    tibble::as_tibble()
  if(nrow(synonyms) == 0){
    synonyms <- diseaseNetwork$synonyms
  }
  
  ## **xref ----
  xref <- toRet$xref %>%
    tibble::as_tibble()
  if(nrow(xref) == 0){
    xref <- diseaseNetwork$xref
  }else{
    xref <- xref  %>%
      dplyr::mutate(ur = paste(pmin(from, to), pmax(from, to), sep = "<->")) %>%
      dplyr::distinct(ur, .keep_all = TRUE)
  }
  
  ## **alt ----
    alt <- toRet$alt %>%
    tibble::as_tibble()
  if(nrow(alt) == 0){
    alt <- diseaseNetwork$alt
  }
    
    
  ## **hierarchy ----
  children <- toRet$child %>%
    tibble::as_tibble()
  if(nrow(children) == 0){
    children <- diseaseNetwork$children
  }
  
  ## **phenotypes ----
  pheno <- toRet$pheno %>%
    tibble::as_tibble()
  if(nrow(pheno) == 0){
    pheno <- diseaseNetwork$pheno
  }
  
  ## **seed ----
  seed <- id
    
  ## **Reset rownames ----
  rownames(nodes) <- NULL
  rownames(synonyms) <- NULL
  rownames(children) <- NULL
  rownames(xref) <- NULL
  rownames(pheno) <- NULL
  
  ## **disNet ----
  diseaseNetwork <- list(nodes = nodes,
                         synonyms = synonyms,
                         children = children,
                         xref = xref,
                         alt = alt,
                         pheno = pheno,
                         seed = seed)
  }

  diseaseNetwork <- structure(diseaseNetwork,
                              class = "disNet")
  return(DODO::normalize_disNet(diseaseNetwork))
}

#========================================================================================@
#========================================================================================@
#' Check validity 
#' 
#' @param x disNet object
#' @param ... additional parameters
#' 
#' @export

is.disNet <- function(x, ...){
  inherits(x, "disNet")  
}


#========================================================================================@
#========================================================================================@
#' Prints a disNet object
#' 
#' @param x disNet object
#' @param ... additional parameters
#' 
#' @export

print.disNet <- function(x, ...){
  
  cat(format(x, ...), sep = "\n")
  
}

#========================================================================================@
#========================================================================================@
#' format disNet
#' 
#' @param x disNet object
#' @param ... additional parameters
#' 
format.disNet <- function(x, ...){
  toRet <-    c(utils::capture.output(x$nodes),
                "",
                "The disNet contains:",
                paste(" - ", 
                      x$nodes %>% 
                        dplyr::filter(grepl("Disease", type)) %>% 
                        nrow(), 
                      "disease nodes from", 
                      length(unique(x$nodes %>% 
                                      dplyr::filter(grepl("Disease", type)) %>% 
                                      dplyr::pull(database))), 
                      "ontologies and",
                      x$nodes %>% 
                        dplyr::filter(grepl("Phenotype", type)) %>% 
                        nrow(), 
                      "phenotype nodes from", 
                      length(unique(x$nodes %>% 
                                      dplyr::filter(grepl("Phenotype", type)) %>% 
                                      dplyr::pull(database))), 
                      "ontologies "),
                paste(" - ", nrow(x$synonyms), "synonyms of the disease nodes"),
                paste(" - ", nrow(x$children), "parent/child edges"),
                paste(" - ", nrow(x$xref), "crossreference edges"),
                paste(" - ", nrow(x$alt), "alternative edges"),
                paste(" - ", nrow(x$pheno), "phenotype edges"),
                paste(" - ", "The disNet was build based on", length(x$seed), "seeds")
  )
  return(toRet)
}

#========================================================================================@
#========================================================================================@
#' Returns the number of nodes in a disNet object
#' 
#' @param x disNet object
#' @param ... additional parameters
#' 
#' @export

length.disNet <- function(x, ...){
  
  nrow(x$nodes)
  
}

#========================================================================================@
#========================================================================================@
#' Returns the dimension of the nodes dataframe in a disNet object
#' 
#' @param x disNet object
#' @param ... additional parameters
#' 
#' @export

dim.disNet <- function(x, ...){
  
  dim(x$nodes)
  
}

#========================================================================================@
#========================================================================================@
#' Subset `[`
#' 
#' @param x disNet object
#' @param ... additional parameters
#' 
#' @export
#' 
'[.disNet' <- function(x, ...){
  stop("Action not allowed on a disNet object")
}

#========================================================================================@
#========================================================================================@
#' Subset `[[`
#' 
#' @param x disNet object
#' @param ... additional parameters
#' 
#' @export
#' 
'[[.disNet' <- function(x, ...){
  
  stop("Action not allowed on a disNet object")
  
  
}

#========================================================================================@
#========================================================================================@
# Slice a disNet
# 
# Choose rows by their ordinal position in the disNet
# 
# @param x disNet object
# @param n Integer row values. Provide either positive values to keep, or 
# negative values to drop. The values provided must be either all positive 
# or all negative. Indices beyond the number of rows in the input are silently ignored.
# 
# @export
# 
# slice_disNet <- function(x,
#                          n){
#   x$nodes <- x$nodes %>%
#     dplyr::slice(n)
#   return(normalize_disNet(x))
#   
# }

#========================================================================================@
#========================================================================================@
#' Merge several disease networks
#' 
#' @param ... disease networks to merge
#' @param list list object with disease networks to merge
#'
#' @return A normalized merged disease network 
#' 
#' @examples 
#' disNet1 <- build_disNet(term = "epilep")
#' disNet2 <- build_disNet(term = "alzheim")
#' mergeDisNet <- c(disNet1, disNet2)
#' 
#' @export
merge_disNet <- function(...,
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
    alt = unique(do.call(rbind, lapply(arg, function(x) x$alt))),
    pheno = unique(do.call(rbind,lapply(arg, function(x) x$pheno))),
    seed = unique(unlist(lapply(arg, function(x) x$seed)))
  )
  disNet <- structure(disNet,
                      class = "disNet")
  return(normalize_disNet(disNet))
}

#========================================================================================@
#========================================================================================@
#' Normalize disease network
#' 
#' This function ensure that disease network properties are fulfilled.
#' 
#' @param x a disease network to normalize
#' 
#' @return A normalized disease network, disNet
#' 
#' @export
#' 
normalize_disNet <- function(x){
  if(!is.null(x$children)){
    x$children <- x$children[which(
      x$children$parent %in% x$nodes$id &
        x$children$child %in% x$nodes$id
    ),]
  }
  if(!is.null(x$xref)){
    x$xref <- x$xref[which(!duplicated(x$xref$ur)),]
    x$xref <- x$xref[which(
      x$xref$from %in% x$nodes$id &
        x$xref$to %in% x$nodes$id
    ),]
  }
  if(!is.null(x$alt)){
    x$alt <- x$alt[which(
      x$alt$id %in% x$nodes$id &
        x$alt$alt %in% x$nodes$id
    ),]
  }
  if(!is.null(x$pheno)){
    x$pheno <- x$pheno[which(
      x$pheno$disease %in% x$nodes$id &
        x$pheno$phenotype %in% x$nodes$id
    ),]
  }
  x$synonyms <- x$synonyms[which(
    x$synonyms$id %in% x$nodes$id
  ),]
  x$seed <- intersect(x$seed, x$nodes$id)
  return(x)
}




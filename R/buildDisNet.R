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
#' query results also providing information on their relationships. A default implementation of the 
#' forward and backward ambiguity facets is present with a limitation only for backward ambiguity 
#' (default: 1 or no ambiguity). In addition, it is possible to avoid returning a particular 
#' hierarchical relationship by specifiying *avoidOrigin*.
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
                        forwardAmbiguity = 10000, 
                        avoidOrigin = NULL,
                        verbose = FALSE){
  if(any(!seed %in% ids)){
    stop("All seed should be in ids")
  }
  ## avoidOrigin 
  if(!is.null(avoidOrigin)){
    match.arg(avoidOrigin, listDB()$db, several.ok = T)
  }
  
  ##
  if(length(ids)==0){
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
  }else{
    ## nodes
    cql <- c('MATCH (n)',
             'WHERE n.name IN $from',
             'RETURN DISTINCT n.name as id, n.label as label, n.definition as definition, ',
             'n.shortID as shortID, n.level as level, labels(n) as type')
    toRet <- call_dodo(neo2R::cypher,
                       prepCql(cql),
                       parameters = list(from = as.list(ids)),
                       result = "row") %>%
      tibble::as_tibble() %>%
      dplyr::mutate(database = gsub(":.*", "", id))
    
    ## synonyms
    cql <- c('MATCH (n)-[r:is_known_as]-(s:Synonym)',
             'WHERE n.name IN $from',
             'RETURN DISTINCT n.name, s.value')
    toRet <- call_dodo(neo2R::cypher,
                       prepCql(cql),
                       parameters = list(from = as.list(ids)))
             
    ## cross-references
    cql <- c("MATCH (n)-[r:is_xref|is_related]-(x)",
             "WHERE n.name IN $from AND x.name IN $from",
             "RETURN DISTINCT n.name as from, x.name as to, Type(r) as type, ",
             "r.FA as forwardAmbiguity, r.BA as backwardAmbiguity")
    toRet <- call_dodo(neo2R::cypher,
                       prepCql(cql),
                       parameters = list(from = as.list(ids)),
                       result = "row")
    
  }
  
  
  
}
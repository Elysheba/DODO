###############################################################################@
#' Extend a disease network
#' 
#' Extending a disNet through specified relationships usingw a
#' specified depth. Ambiguity of cross-references can be taken into account
#' forward and/or backward (probably more relevant than forward).
#' Optionally a set of disease nodes can be avoided during the extension.
#' 
#' @param disNet a disease network
#' @param relations the kind of relationships to take into account.
#' Possible values among combinations of: "xref", "child", "parent", and/or "pheno".
#' (Default value: "xref")
#' @param step the number of steps to take in the search (default: 10000 ==> ~exhaustive)
#' @param FA.transitivity forward ambiguity while using transitivity to identify cross-references (default: no filter)
#' @param BA.transitivity backward ambiguity while using transitivity to identify cross-references (default: 1)
#' @param FA.non_transitivity forward ambiguity while using transitivity to identify cross-references (default: no filter)
#' @param BA.non_transitivity backward ambiguity while using transitivity to identify cross-references (default: no filter)
#' @param avoidNodes a vector of disease ids to be avoided in the search path
#' @param avoidOrigin: allows to avoid traversing parent/child edges from a particular ontology (default = NULL)
#' @param verbose show query input (default = FALSE)
#' 
#' @details 
#' 
#'  Once a initial disNet has been constructed, it can be extended through cross-reference or hierarchical information 
#'  through a defined number of steps (default is 10,000 steps). While extending, a vector of identifiers can be provided
#'  through which one does not want to traverse (*avoidNodes*) or resource exluded when traversing and returning 
#'  hierarchical information (*avoidOrigin*). The issue of ambiguity arises when traversing cross-reference edges as 
#'  different ontology use broad or narrow definition for diseases. Two adaptations are provided 
#'  to allow controlling the transitivity between nodes. 
#'  
#'  To deal with ambiguity between ontologie cross-reference edges, the property of forward and backward ambiguity is defined. 
#'  By default, filtering is only put in place on backward ambiguity to avoid moving from a narrow concept to a broader concept. 
#'  This can result in 'hopping' through more broader defined nodes, more distantly related to the initial concept. 
#'  Filtering on forward ambiguity is not advised by default but is equivalent to moving from a more broadly defined disease 
#'  concept to several narrower disease concepts in another ontology. 
#'  
#'  It is also possible to extend to phenotype information with some limitations. 
#'  The basis of *DODO* is the objective to travers the different disease ontologies efficiently and exhaustively. 
#'  A disNet can be annotated with phenotype information using the "pheno" as additional property for *relations*.
#'  However, obtaining diseases associated to a set of phenotype identifiers cannot directly be done using the extendDisNet. 
#'  This can be done using the *convertConcept* function first, then followed by *extendDisNet* (see vignette). 
#' 
#' @return A normalized disease network 
#' 
#' @examples 
#' disNet <- buildDisNetByTerm("epilep",fields = c("synonym","label","definition"))
#' extDisNet <- extendDisNet(disNet,relations = c("child","xref"), toAvoid = "MONDO:0100033")
#' 
#' @seealso buildDisNet
#' 
#' @export
#'
extendDisNet <- function(
  disNet,
  relations = "xref",
  step = NULL,
  FA.transitivity = NULL,
  BA.transitivity = 1,
  FA.non_transitivity = NULL,
  BA.non_transitivity = NULL,
  avoidNodes = NULL,
  avoidOrigin = NULL
  ){
  ## checking
  relations <- match.arg(relations, 
                         c("xref", "child", "parent", "phenotype", "disease"), 
                         several.ok=TRUE)
  ##
  stopifnot(step > 0,
            is.disNet(disNet))
  if("parent" %in% relations && "child" %in% relations){stop("Both parent and child in relations, pick one...")}

  ## avoidOrigin 
  if(!is.null(avoidOrigin)){
    match.arg(avoidOrigin, listDB()$db, several.ok = T)
  }
  ## ambiguity
  stopifnot(is.null(FA.transitivity) || FA.transitivity == 1,
            is.null(BA.transitivity) || BA.transitivity == 1,
            is.null(FA.non_transitivity) || FA.non_transitivity == 1, 
            is.null(BA.non_transitivity) || BA.non_transitivity == 1)
  
  # if(is.null(BA.transitivity)){
  #   transitivity <- "is_xref"
  # }else{
  #   transitivity <- "is_xref_nba"
  # }
  # 
  # if(is.null(BA.non_transitivity)){
  #   relationship <- "is_related|is_xref"
  # }else{
  #   relationship <- "is_related_nba|is_xref_nba"
  # }
   
  ##############################################@
  ## nodes ----
  nodes <- disNet$nodes
  seed <- disNet$seed
  ##
  ids <- disNet$nodes$id
  # ids <- try(findID(ids = ids), silent = TRUE)
  if(!is.null(avoidNodes)){
    ids <- setdiff(ids, avoidNodes)
  }
  # if(class(ids) == "try-error"){
  #   stop(c("No nodes to extend from"))
  # }
  # 
  ##############################################@
  ## xref/parent/child ----
  if(c("xref", "parent", "child") %in% relations){
      ###################@
      ## build relationship
      
      ## transitivity
      q <- NULL
      if("xref" %in% relations){
        if(is.null(BA.transitivity)){
          transitivity <- "is_xref"
        }else{
          transitivity <- "is_xref_nba"
        }
        q <- c(q, transitivity)
      }
      if("parent" %in% relations){q <- c(q, "|is_a>")}
      if("child" %in% relations){q <- c(q, "|<is_a")}
      q <- paste0(q, collapse = ">")
      
      ## Non-transitivity
      if(is.null(BA.non_transitivity)){
        relationship <- "is_related|is_xref"
      }else{
        relationship <- "is_related_nba|is_xref_nba"
      }
      
      ##############@
      ## build query
      cql.xref <- c(
        'MATCH (f)',
        ifelse(is.null(avoidNodes), 
               'WHERE f.name IN $from', 
               'MATCH (a:Concept) WHERE f.name IN $from AND a.name IN $avoid'),
        'CALL apoc.path.expandConfig(',
        sprintf('f, {uniqueness:"NODE_GLOBAL", relationshipFilter:"%s", maxLevel:-1 %s}',
                q,
                # ifelse(is.null(step), -1, step-1),
                ifelse(is.null(avoidNodes), "", ", blacklistNodes:a")),
        ') YIELD path',
        'WITH nodes(path) AS e',
        sprintf('MATCH (e1:Concept)-[r:%s]->(e2:Concept) WHERE e1 IN e %s',
                relationship,
                ifelse(is.null(avoidNodes), "", "AND NOT e1.name IN $avoid AND NOT e2.name IN $avoid")),
        # 'RETURN DISTINCT',
        # 'e.name AS from, e2.name AS to'
        # 'UNWIND nodes(path) as n',
        'RETURN DISTINCT e1.name, e2.name, r.FA, r.BA'
      )
      
      
      
      
      ## Call
      toRet <- call_dodo(
          neo2R::cypher,
          prepCql(cql),
          parameters = list(from = as.list(ids),
                            avoid = as.list(avoidNodes)),
          result = "row") %>%
        unlist() %>%
        unname() 
    }
    
    ##############@
    ## build query xref non-transitive
    if(step == 1){
      toRet <- ids
    }
    
    if("xref" %in% relations){
      cql <- c(
        sprintf('MATCH (e1)-[r:%s]->(e2) WHERE e1.name IN $from %s',
                relationship,
                ifelse(is.null(avoidNodes), "", "AND NOT e1.name IN $avoid AND NOT e2.name IN $avoid")),
        'RETURN DISTINCT',
        'e1.name AS from, e2.name AS to, r.FA AS forwardAmbiguity, r.BA AS backwardAmbiguity'
      )
      xref <- call_dodo(
          neo2R::cypher,
          prepCql(cql),
          parameters = list(from = as.list(toRet),
                            avoid = as.list(avoidNodes)),
          result = "row") %>%
        as_tibble()
      if(nrow(xref) == 0){
        xref <- tibble::tibble(
          from = character(),
          to = character(),
          forwardAmbiguity = numeric(),
          backwardAmbiguity = numeric(),
          ur = character() 
        )
      }else{
        xref <- xref  %>%
          dplyr::mutate(ur = paste(pmin(from, to), pmax(from, to), sep = "<->")) %>%
          dplyr::distinct(ur, .keep_all = TRUE)
      }
    }
  
    ##############@
    ## build query parent/child
    if(any(c("parent","child") %in% relationship)){
      cql <- c(
        sprintf('MATCH (p)%s(c)',
                ifelse("child" %in% relationship, "-[r:is_a]->", "<-[r:is_a]-")),
        sprintf('WHERE p.name in $from %s %s',
                ifelse(step == 1, "", "AND c.name in $from"),
                ifelse(is.null(avoidOrigin), "", "AND NOT r.origin in $avoid")),
        'RETURN DISTINCT p.name as parent, c.name as child, r.origin as origin'
      )
      child <- call_dodo(
          neo2R::cypher,
          prepCql(cql),
          parameters = list(from = as.list(toRet),
                            avoid = as.list(avoidOrigin)),
          result = "row") %>%
        as_tibble()
      if(nrow(child) == 0){
        child = tibble::tibble(
          parent = character(),
          child = character(),
          origin = character()
        )
      }
    }
  }
  
  ##############################################@
  ## phenotype ----
  if("phenotype" %in% relations){
    cql <- c(
        'MATCH (d:Disease)-[:has_pheno]->(p:Phenotype)',
        'WHERE d.name IN $from',
        "RETURN DISTINCT d.name as disease, p.name as phenotype"
      )
    phenotype <- call_dodo(
        neo2R::cypher,
        prepCql(cql),
        parameters = list(from = as.list(ids)),
        result = "row") %>%
      as_tibble()
    if(nrow(phenotype) == 0){
      pheno = tibble::tibble(
        disease = character(),
        phenotype = character()
      )
    }
  }
  
  ##############################################@
  ## disease ----
  if("disease" %in% relations){
    cql <- c(
      'MATCH (p:Phenotype)<-[:has_pheno]-(d:Disease)',
      'WHERE p.name IN $from',
      "RETURN DISTINCT d.name as disease, p.name as phenotype"
    )
    disease <- call_dodo(
        neo2R::cypher,
        prepCql(cql),
        parameters = list(from = as.list(ids)),
        result = "row") %>%
      as_tibble()
    if(nrow(disease) == 0){
      pheno = tibble::tibble(
        disease = character(),
        phenotype = character()
      )
    }
  }
  
  
  
  ##############################################@
  ## nodes
  cql <- c('MATCH (n)',
           'WHERE n.name IN $from',
           'RETURN DISTINCT n.name as id, n.label as label, n.definition as definition, ',
           'n.shortID as shortID, n.level as level, labels(n) as type')
  nodes <- call_dodo(neo2R::cypher,
                     prepCql(cql),
                     parameters = list(from = as.list(c(ids, 
                                                        child$parent, child$child, 
                                                        xref$from, xref$to,
                                                        ))),
                     result = "row") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(database = gsub(":.*", "", id))
  
  
  ##############################################@
  ## Reset rownames
  rownames(nodes) <- NULL
  rownames(synonyms) <- NULL
  rownames(children) <- NULL
  rownames(parent) <- NULL
  rownames(xref) <- NULL
  rownames(phenotype) <- NULL
  rownames(disease) <- NULL
  ##
  diseaseNetwork <- list(nodes = nodes,
                         synonyms = synonyms,
                         children = child,
                         xref = xref,
                         pheno = pheno,
                         seed = seed)
  diseaseNetwork <- structure(diseaseNetwork,
                              class = "disNet")
  return(diseaseNetwork)
}
#========================================================================================@
#========================================================================================@
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
#' @param transitive.ambiguity backward ambiguity while using transitivity to identify cross-references (default: 1)
#' @param intransitive.ambiguity backward ambiguity while using transitivity to identify cross-references (default: no filter)
#' @param avoidNodes a vector of disease ids to be avoided in the search path
# @param avoidOrigin: allows to avoid traversing parent/child edges from a particular ontology (default = NULL)
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
extend_disNet <- function(
  disNet,
  relations = "xref",
  step = NULL,
  transitive.ambiguity = 1,
  intransitive.ambiguity = NULL,
  avoidNodes = NULL
  # avoidOrigin = NULL
  ){
  ## checking
  relations <- match.arg(relations, 
                         c("xref", "child", "parent", "phenotype", "disease", "alt"), 
                         several.ok = TRUE)
  stopifnot(step > 0, is.disNet(disNet),
            length(disNet) > 0)
  if("parent" %in% relations && "child" %in% relations){
    stop("Both parent and child in relations, pick one...")}
  if("disease" %in% relations && "phenotype" %in% relations){
    stop("Both phenotype and disease extension in relations, pick one...")}
  
  ## avoidOrigin 
  # if(!is.null(avoidOrigin)){match.arg(avoidOrigin, list_database()$database, several.ok = T)}
  ## ambiguity
  stopifnot(is.null(transitive.ambiguity) || transitive.ambiguity == 1,
            is.null(intransitive.ambiguity) || intransitive.ambiguity == 1)
  ## initiate empty df
  pheno <- tibble(disease = character(),
                  phenotype = character())
  syn <- tibble(id = character(),
                synonym = character())
  alt <- tibble(id = character(),
                alt = character())
  xref <- tibble(from = character(),
                 to = character(),
                 forwardAmbiguity = integer(),
                 backwardAmbiguity = integer())
  children = tibble(child = character(),
                    parent = character(),
                    origin = character())
  
  ##############################################@
  ## ids ----
  nodes <- disNet$nodes
  seed <- disNet$seed
  ##
  ids <- disNet$nodes$id
  if(!is.null(avoidNodes)){
    ids <- setdiff(ids, avoidNodes)
  }
   
  ##############################################@
  ## build transitivity ----
  q <- NULL
  if("xref" %in% relations){
    if(is.null(transitive.ambiguity)){
      transitive <- "is_xref>"
    }else{
      transitive <- "is_xref_nba>"
    }
    q <- c(q, transitive)
  }
  if("parent" %in% relations){q <- c(q, "is_a>")}
  if("child" %in% relations){q <- c(q, "<is_a")}
  if("alt" %in% relations){q <- c(q, "is_alt")}
  q <- paste0(q, collapse = "|")
  
  ##############@
  ## steps ----
  if(is.null(step)){
    cql.trans <- c(
      'MATCH (f)',
      ifelse(is.null(avoidNodes), 
             'WHERE f.name IN $from', 
             'MATCH (a:Concept) WHERE f.name IN $from AND a.name IN $avoid'),
      'CALL apoc.path.expandConfig(',
      sprintf('f, {uniqueness:"NODE_GLOBAL", relationshipFilter:"%s", maxLevel:-1 %s}',
              q,
              ifelse(is.null(avoidNodes), "", ", blacklistNodes:a")),
      ') YIELD path',
      'UNWIND nodes(path) as n',
      'WITH DISTINCT COLLECT(n) as node',
      sprintf('MATCH (n:Concept)-[r:%s]->(n1:Concept) WHERE n IN node AND n1 IN node',
              gsub(paste(">","<", sep = "|"), "", q)),
      'RETURN DISTINCT n.name as from, n1.name as to, Type(r) as relation,',
      'r.origin as origin, r.BA as backwardAmbiguity, r.FA as forwardAmbiguity,',
      'Type(r) as type'
    )
              
    
    ## run transitivity ----
    transitivity <- DODO::call_dodo(
        neo2R::cypher,
        prepCql(cql.trans),
        parameters = list(from = as.list(ids),
                          avoid = as.list(avoidNodes)),
        result = "row")
    
    if(!is.null(transitivity)){
      xref <- transitivity %>%
        dplyr::filter(grepl("xref", relation)) %>%
        dplyr::select(from,
                      to, 
                      forwardAmbiguity,
                      backwardAmbiguity, 
                      type)
      children <- transitivity %>%
        dplyr::filter(grepl("^is_a$", relation)) %>%
        dplyr::select(child = from,
                      parent = to,
                      origin)
      alt <- transitivity %>%
        dplyr::filter(grepl("is_alt", relation)) %>%
        dplyr::select(id = to, 
                      alt = from) 
      ids <- unique(c(transitivity$from, transitivity$to))
    }
    rm(cql.trans)
  }else{
    ids <- ids
  }
  
  ##############@
  ## intransitive relationships ----
  ## **xref ----
  if("xref" %in% relations){
    if(is.null(intransitive.ambiguity)){
      intransitive <- "is_related|is_xref"
    }else{
      intransitive <- "is_related_nba|is_xref_nba"
    }
    cql.xref <- c(
      sprintf('MATCH (e1:Concept)-[r:%s]->(e2:Concept) WHERE e1.name IN $from %s',
              intransitive,
              ifelse(is.null(avoidNodes), "", "AND NOT e1.name IN $avoid AND NOT e2.name IN $avoid")),
      'RETURN DISTINCT',
      'e1.name AS from, e2.name AS to, r.FA AS forwardAmbiguity, r.BA AS backwardAmbiguity,',
      "Type(r) as type"
    )
  }
  ## **children ----
  if(any(c("child", "parent") %in% relations)){
    cql.child <- c(
      'MATCH (c:Concept)-[r:is_a]->(p:Concept)',
      sprintf('WHERE %s.name in $from',
              ifelse("child" %in% relations, "p", "c")),
              # ifelse(is.null(avoidOrigin), "", "AND NOT r.origin in $origin")),
      'RETURN DISTINCT p.name as parent, c.name as child, r.origin as origin'
    )
  }
  ## **pheno ----
  if(any(c("disease", "phenotype") %in% relations)){
    cql.pheno <- c('MATCH (d:Disease)-[r:has_pheno]->(p:Phenotype)',
                   sprintf('WHERE %s.name IN $from',
                           ifelse("phenotype" %in% relations, "d", "p")),
                   'RETURN DISTINCT d.name as disease, p.name as phenotype')
  }
  
  ## Gather queries ----
  if(any(c("disease", "phenotype", "child", "parent", "xref") %in% relations)){
    s <- grep("cql", ls(), value = TRUE)
    statements <- sapply(s,
                         function(x){
                           prepCql(get(x))
                         },
                         USE.NAMES = FALSE,
                         simplify = FALSE)
    names(statements) <- gsub("cql.", "", s)
    
    ## Call query ----
    toRet <- call_dodo(
      neo2R::multicypher,
      queries = statements,
      parameters = list(from = as.list(ids),
                        avoid = as.list(avoidNodes)),
                        # origin = as.list(avoidOrigin)),
      result = "row"
    )
  }
  
  ###############@
  ## Gather ----
  ## **xref ----
  if("xref" %in% relations){
    xref <- dplyr::bind_rows(xref,
                             toRet$xref) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(ur = paste(pmin(from, to), pmax(from, to), sep = "<->")) %>%
      dplyr::distinct(ur, .keep_all = TRUE)
  }
  ## **children ----
  if(any(c("child", "parent") %in% relations)){
    children <- dplyr::bind_rows(children,
                                  toRet$child) %>%
      tibble::as_tibble() %>%
      dplyr::distinct()
  }
  
  ## **pheno ----
  if(any(c("phenotype", "disease") %in% relations)){
    if(!is.null(toRet$pheno)){
      pheno <- toRet$pheno %>%
        tibble::as_tibble() %>%
        dplyr::distinct()
    }
  }

  ###########################################@
  ## get additional step of alt id ----
  ids <- unique(c(xref$from, xref$to, 
                  children$child, children$parent, 
                  pheno$disease, pheno$phenotype,
                  alt$id, alt$alt))
  if("alt" %in% relations){
    cql.alt <- c(
      'MATCH (n:Concept)<-[r:is_alt]-(a:Concept)',
      'WHERE n.name in $from',
      'RETURN DISTINCT n.name as id, a.name as alt'
    )
    if(nrow(alt) != 0){
      alt <- call_dodo(cypher,
                       prepCql(cql.alt),
                       result = "row",
                       parameters = list(from = as.list(ids))) %>%
        tibble::as_tibble() %>%
        dplyr::distinct()
    }
  }
  
  ###########################################@
  ## Build disNet ----
  ids <- unique(c(ids, alt$alt))
  cql.nodes <- c('MATCH (n:Concept)',
                 'WHERE n.name IN $from',
                 'RETURN DISTINCT n.name as id, n.label as label, n.definition as definition, ',
                 'n.shortID as shortID, n.level as level, labels(n) as type')
  cql.syn <- c('MATCH (n:Concept)-[r:is_known_as]->(s:Synonym)',
               'WHERE n.name IN $from',
               'RETURN DISTINCT n.name as id, s.value as synonym')

  ## Gather queries ----
  s <- grep(paste("cql.nodes", "cql.syn", "cql.alt", sep = '|'), 
            ls(), value = TRUE)
  statements <- sapply(s,
                       function(x){
                         prepCql(get(x))
                       },
                       USE.NAMES = FALSE,
                       simplify = FALSE)
  names(statements) <- gsub("cql.", "", s)
  
  toRet <- call_dodo(
    neo2R::multicypher,
    queries = statements,
    result =  "row",
    parameters = list(from = as.list(ids))
  )
  
  ## **nodes ----
  nodes <- toRet$nodes %>%
    tibble::as_tibble() %>%
      dplyr::mutate(database = gsub(":.*", "", id))
  
  ## **synonyms ----
  if(!is.null(toRet$syn)){
   synonyms <- toRet$syn %>%
      tibble::as_tibble()
  }


  ##############################################@
  ## Reset rownames ----
  rownames(nodes) <- NULL
  rownames(synonyms) <- NULL
  rownames(children) <- NULL
  rownames(xref) <- NULL
  rownames(alt) <- NULL
  rownames(pheno) <- NULL
  
  ##############################################@
  ## Disnet ----
  diseaseNetwork <- list(nodes = nodes,
                         synonyms = synonyms,
                         children = children,
                         xref = xref,
                         alt = alt,
                         pheno = pheno,
                         seed = seed)
  diseaseNetwork <- structure(diseaseNetwork,
                              class = "disNet")
  return(normalize_disNet(diseaseNetwork))
}

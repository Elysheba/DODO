#' Convert concept identifiers
#' 
#' Another core functionality is the ability to convert concept identifiers between different 
#' databases and types (disease or phenotype). By default a backward ambiguity of one is taken,
#' with no limitation on forward ambiguity. 
#' 
#' @param from a vector with identifier to convert formatted as DB:id (eg. "MONDO:0005027)
#' @param to database to convert to (default = NULL, no filtering)
#' @param from.concept concept (disease or phenotype) of from
#' @param to.concept concept (disease or phenotype) to convert to
#' @param FA forward ambiguity (default: no filter)
#' @param BA backward ambiguity (default: BA = 1)
#' @param FA.transitivity forward ambiguity while using transitivity to identify cross-references (default: no filter)
#' @param BA.transitivity backward ambiguity while using transitivity to identify cross-references (default: 1)
#' @param FA.non_transitivity forward ambiguity while using transitivity to identify cross-references (default: no filter)
#' @param BA.non_transitivity backward ambiguity while using transitivity to identify cross-references (default: no filter)
#' @param verbose show query input (default: FALSE)
#' 
#' @return a dataframe with three columns:
#' - from: identifier to convert
#' - to: returned conversion
#' - hops: length of the shortest path between them
#' 
#' @export
#' 
convertConcept <- function(from, 
                       to = NULL,
                       from.concept = c("Disease", "Phenotype"),
                       to.concept = c("Disease", "Phenotype"),
                       FA.transitivity = NULL,
                       BA.transitivity = 1,
                       FA.non_transitivity = NULL,
                       BA.non_transitivity = NULL,
                       verbose = FALSE){
   from.concept <- match.arg(from.concept, c("Disease", "Phenotype"))
   from <- setdiff(from, NA)
   stopifnot(is.character(from), length(from)>0)
   
   stopifnot(is.null(FA.transitivity) || FA.transitivity == 1,
             is.null(BA.transitivity) || BA.transitivity == 1,
             is.null(FA.non_transitivity) || FA.non_transitivity == 1, 
             is.null(BA.non_transitivity) || BA.non_transitivity == 1)
   
   ## add removing edges for FA (within upload)
   
   if(is.null(BA.transitivity)){
      transitivity <- "is_xref"
   }else{
      transitivity <- "is_xref_nba"
   }
   
   if(is.null(BA.non_transitivity)){
      relationship <- ":is_related|is_xref*0..1"
   }else{
      relationship <- ":is_related_nba|is_xref_nba*0..1"
   }
   
   #############################################@
   ## To convert identifiers between concepts, first they are converted within the concept (d-d, p-p) = B1 
   ## followed by a conversion between concepts (d-p, p-d) = B2
   
   ## Convert within concepts: B1
   cql <- c(
      sprintf('MATCH (f:%s) WHERE f.name IN $from',
         from.concept),
      'CALL apoc.path.expandConfig(',
      sprintf('f, {uniqueness:"NODE_GLOBAL", relationshipFilter:"%s>"}',
              transitivity),
      ') YIELD path',
      'WITH (nodes(path))[0] as s, last(nodes(path)) as e',
      sprintf('MATCH (e)-[%s]->(e2)',
              relationship),
      'RETURN DISTINCT',
      's.name as from, e2.name as to'
      # '(nodes(path))[0].name AS from, ',
      # 'last(nodes(path)).name AS to, ',
      # 'size(relationships(path)) AS hops'
   )
   b1 <- call_dodo(
      neo2R::cypher,
      prepCql(cql),
      parameters = list(from = as.list(from)),
      result = "row")
   
   # cql <- c(
   #    sprintf(
   #       'MATCH (f:%s)-[%s]-(t:%s) WHERE f.name in $from',
   #       from.concept,
   #       relationship,
   #       from.concept),
   #    'RETURN DISTINCT f.name AS from, t.name AS to'
   #    )
   # b1.related <- call_dodo(
   #       neo2R::cypher,
   #       prepCql(cql),
   #       parameters = list(from = as.list(unique(b1.xref$to))),
   #       result = "row") %>%
   #    dplyr::mutate(hops = NA) %>%
   #    dplyr::left_join(b1.xref %>% dplyr::select(ori = from, to),
   #                     by = c("from" = "to")) %>%
   #    dplyr::select(from = ori,
   #                  to,
   #                  hops)
   # toRet <- dplyr::bind_rows(b1.xref,
   #                           b1.related) %>%
   #    tibble::as_tibble() %>%
   #    dplyr::distinct()
   
   #############################################@
   ## Convert between concepts: B2
   if(from.concept != to.concept){
      cql <- c(
         sprintf('MATCH (f:%s)-[:has_pheno]-(t:%s)',
                 from.concept,
                 to.concept),
         'WHERE f.name IN $from',
         "RETURN DISTINCT f.name as from, t.name as to"
      )
      b2 <- call_dodo(
         neo2R::cypher,
         prepCql(cql),
         parameters = list(from = as.list(from)),
         result = "row"
      )
   }
   
   toRet <- dplyr::bind_rows(b1,
                             b2) %>%
      tibble::as_tibble() %>%
      dplyr::distinct()
   
   return(toRet)
}

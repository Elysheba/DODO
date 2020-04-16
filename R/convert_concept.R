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
#' @param deprecated include deprecated identifiers (default: false)
#' @param transitive_ambiguity backward ambiguity while using transitivity to identify 
#' cross-references (default: 1)
#' @param intransitive_ambiguity specification for backward ambiguity used in the final 
#' step of conversion (default: no filter)
#' @param step number of steps to traverse when converting within concepts (default: NULL) 
#' (see details)
#' 
#' @details Conversion is performed in different steps, first identifiers are converted 
#' (if requested) within a concept, otherwise stated, their cross-references are 
#' returned depending on the provided parameters of transitive_ambiguity and
#' intransitive_ambiguity. The manner of converting within concepts is defined by the *step*
#' parameters (see below). Next, these identifiers are converted between concepts 
#' (Disease -> Phenotype or Phenotype -> Disease).
#' 
#' If deprecated = TRUE, only the deprecated identifiers of the original input 
#' (from) are returned. 
#' 
#' The step parameters allows the user to specify the number of steps to take when 
#' onverting within a concept (e.g. Disease or Phenotype). If step = NA, 
#' no conversion within concepts is performed (skipping the first step). If step = NULL
#' the full transitivity mappings are used to return cross-references edges. This means that all
#' identifiers are converted using the transitivity between *is_xref* mappings. This is followed
#' by another step where all cross-references one step removed are returned using both *is_xref*
#' and *is_related* edges. The transitive_ambiguity and intransitive_ambiguity 
#' can be used to specify how/which cross-reference identifiers should be returned by 
#' selecting edges below the ambiguity thresholds to traverse.
#' If step = 1, only direct cross-references are returned using both *is_xref* and *is_related* edges. 
#' This is similar to the final step of the full conversion (step = NULL) and the intransitive_ambiguity 
#' can be used to specify the threshold on the backward ambiguity. 
#' 
#' @return a dataframe with three columns:
#' - from: identifier to convert
#' - to: returned conversion
#' 
#' @examples 
#' convert_concept(from = "MONDO:0005027",
#'                 to = "EFO", 
#'                 from.concept = "Disease",
#'                 to.concept = "Disease")
#' @export
#' 
convert_concept <- function(from, 
                            to = NULL,
                            from.concept = c("Disease", "Phenotype"),
                            to.concept = c("Disease", "Phenotype"),
                            deprecated = FALSE,
                            transitive_ambiguity = 1,
                            intransitive_ambiguity = NULL,
                            step = NULL){
   from.concept <- match.arg(from.concept, c("Disease", "Phenotype"), several.ok = FALSE)
   to.concept <- match.arg(to.concept, c("Disease", "Phenotype"), several.ok = FALSE)
   db.to <- match.arg(to, c(NULL, list_database()$database))
   from <- setdiff(from, NA)
   stopifnot(is.character(from), length(from)>0,
             is.null(transitive_ambiguity) || transitive_ambiguity == 1,
             is.null(intransitive_ambiguity) || intransitive_ambiguity == 1,
             is.null(step) || step == 1 || is.na(step))
   
   if(is.null(transitive_ambiguity)){
      transitivity <- "is_xref>"
   }else{
      transitivity <- "is_xref_nba"
   }
   
   if(is.null(intransitive_ambiguity)){
      relationship <- ":is_related|is_xref*0..1"
   }else{
      relationship <- ":is_related_nba|is_xref_nba*0..1"
   }
   
   b1 <- b2 <- b3 <- toRet <- tibble::tibble(from = character(),
                                             to = character(),
                                             deprecated = logical())
   
   if(!deprecated & !isTRUE(is.na(step))){
      #############################################@
      ## To convert identifiers between concepts, first they are converted within the concept (d-d, p-p) = B1 
      ## followed by a conversion between concepts (d-p, p-d) = B2
      
      if(isTRUE(step == 1)){
         cql <- c(
            sprintf('MATCH (f:%s)-[%s]-(t:%s)',
                    from.concept,
                    relationship,
                    from.concept),
            'WHERE f.name IN $from',
            "RETURN DISTINCT f.name as from, t.name as to"
         )
         toRet <- call_dodo(
               neo2R::cypher,
               neo2R::prepCql(cql),
               parameters = list(from = as.list(from)),
               result = "row") %>%
            tibble::as_tibble()
         
      }else{
         ## Convert within concepts: B1
         cql <- c(
            sprintf('MATCH (f:%s)-[%s]-(t:%s) WHERE f.name IN $from',
               from.concept,
               relationship,
               from.concept),
            'CALL apoc.path.expandConfig(',
            sprintf('f, {uniqueness:"NODE_GLOBAL", relationshipFilter:"%s>"}',
                    transitivity),
            ') YIELD path',
            'WITH f as f, (nodes(path))[0] as s, last(nodes(path)) as e',
            sprintf('MATCH (e)-[%s]->(e2)',
                    relationship),
            'RETURN DISTINCT',
            's.name as from, e2.name as to')
            # (nodes(path))[0].name AS from
            # last(nodes(path)).name AS to
            # size(relationships(path)) AS hops
         toRet <- call_dodo(
               neo2R::cypher,
               neo2R::prepCql(cql),
               parameters = list(from = as.list(from)),
               result = "row") %>%
            tibble::as_tibble()
      }
      
      if(nrow(toRet) != 0){
         b1 <- toRet
      }
   }else{
      b1 <- tibble::tibble(from = from,
                   to = from)
   }
   
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
         parameters = list(from = as.list(unique(b1$to))),
         result = "row"
      ) %>% tibble::as_tibble()
   }
   
   if(nrow(b2) != 0){
    toRet <- dplyr::bind_rows(b1,
             dplyr::inner_join(b1,
                               b2,
                               by = c("to" = "from")) %>%
             dplyr::select(from,
                           to = to.y)) %>%
       tibble::as_tibble() %>%
       dplyr::distinct() %>%
       dplyr::mutate(deprecated = FALSE)
   }
   
   #############################################@
   ## Return deprecated identifiers

   if(deprecated){
      cql <- c(
         'MATCH (f:Concept)<-[:is_alt]-(t:Concept)',
         'WHERE f.name IN $from',
         "RETURN DISTINCT f.name as from, t.name as to"
      )
      b3 <- call_dodo(
         neo2R::cypher,
         prepCql(cql),
         parameters = list(from = as.list(from)),
         result = "row"
      ) %>%
         tibble::as_tibble()
      if(nrow(b3) != 0){
         b3 <- b3 %>%
            dplyr::distinct() %>%
            dplyr::mutate(deprecated = TRUE)
      }
   }

   toRet <- dplyr::bind_rows(toRet,
                             b3)
   
   if(!is.null(to)){
      toRet <- toRet %>% 
         dplyr::filter(grepl(glue::glue("{db.to}"), to))
   }
   
   return(toRet %>% dplyr::distinct())
}

####################################@
#' Get related concepts
#' 
#' The specific conversion procedure is recommended for the ontologies that 
#' consider disease concepts that are (only) connected through *is_related* edges
#' (e.g. ICD10, ICD9).
#' 
#' @param from a vector with identifier to convert formatted as DB:id (eg. "MONDO:0005027)
#' @param to database to convert to (default = NULL, no filtering)
#' @param from.concept concept (disease or phenotype) of from
#' @param to.concept concept (disease or phenotype) to convert to
#' @param deprecated include deprecated identifiers (default: false)
#' @param transitive_ambiguity backward ambiguity while using transitivity 
#' to identify cross-references (default: 1)
#' @param intransitive_ambiguity specification for backward ambiguity used 
#' in the final step of conversion (default: no filter)
#' @param step number of steps to traverse when converting within concepts (default: NULL) 
#' (see details)
#' 
#' @details The specific conversion procedure is recommended for the ontologies that 
#' consider disease concepts that are (only) connected through *is_related* edges
#' (e.g. ICD10, ICD9). This limits or removes the effect of the transitive 
#' mapping and not return all cross-references. Therefore, for these ontologies it 
#' is recommend that in addition to the standard conversion, an additional step 
#' of intransitive mapping is performed which is implemented in the function *get_related*.
#' This is implemented in a symmetric way, where first the intransitive mapping
#' is returned (identical to the final step of a normal conversion with step = NULL
#' where the same ambiguity specifications defined by 
#' *intransitive_ambiguity*). Next the standard conversion is applied with a
#' transitive mapping and final step where all cross-references one step removed 
#' are returned using both *is_xref* and *is_related* edges.
#' 
#' @return a dataframe with three columns:
#' - from: identifier to convert
#' - to: returned conversion
#' - deprecated: indicating whether the returned "to" identifier is a deprecated ID
#' 
#' @seealso convert_concept
#' 
#' @examples
#' toRet <- get_related(from = "ICD10:G40.9",
#'             to = "DOID",
#'             from.concept = "Disease", 
#'             to.concept = "Disease")
#' 
#' @export
#' 
get_related <- function(from, 
                        to = NULL,
                        from.concept = c("Disease", "Phenotype"),
                        to.concept = c("Disease", "Phenotype"),
                        deprecated = FALSE,
                        transitive_ambiguity = 1,
                        intransitive_ambiguity = NULL,
                        step = NULL){
   from.concept <- match.arg(from.concept, c("Disease", "Phenotype"), several.ok = FALSE)
   to.concept <- match.arg(to.concept, c("Disease", "Phenotype"), several.ok = FALSE)
   db.to <- match.arg(to, c(NULL, list_database()$database))
   from <- setdiff(from, NA)
   stopifnot(is.character(from), length(from)>0,
             is.null(transitive_ambiguity) || transitive_ambiguity == 1,
             is.null(intransitive_ambiguity) || intransitive_ambiguity == 1,
             is.null(step) || step == 1 || is.na(step))
   
   if(is.null(intransitive_ambiguity)){
      relationship <- ":is_related|is_xref*0..1"
   }else{
      relationship <- ":is_related_nba|is_xref_nba*0..1"
   }
   ## Additional step
   cql <- c(sprintf("MATCH (n:Disease)-[%s]-(n1:Disease)", relationship),
            "WHERE n.name in $from",
            "RETURN n.name as from2, n1.name as to2")
   relConv <- call_dodo(neo2R::cypher, 
                        neo2R::prepCql(cql),
                        parameters = list(from = as.list(unique(from))),
                        result = "row")
   ## normal conversion to end
   dodoConv <- convert_concept(from = relConv$to2,
                               to = to,
                               from.concept = from.concept,
                               to.concept = to.concept,
                               deprecated = deprecated,
                               transitive_ambiguity = transitive_ambiguity,
                               intransitive_ambiguity = intransitive_ambiguity)

   finConv <- dodoConv %>%
      dplyr::left_join(relConv,
                by = c("from" = "to2")) %>%
      dplyr::select(from = from2,
             to = to) %>%
      dplyr::mutate(deprecated = FALSE) %>%
      dplyr::distinct()
   return(finConv)
}



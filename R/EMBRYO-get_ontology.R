#' Embryonic function to get full ontology
#' 
#' @export
#' 
get_ontology <- function(database, concept=c("Disease", "Phenotype")){
   concept <- match.arg(concept)
   ## Get concepts ----
   cql <- c(
      sprintf(
         'MATCH (c:%s)-[:is_in]->(:Database {name:$database})',
         concept
      ),
      'RETURN DISTINCT c.name AS name, c.shortID AS shortID, c.label AS label,',
      'c.definition AS definition, c.level AS level'
   )
   concepts <- call_dodo(
      neo2R::cypher,
      prepCql(cql),
      parameters=list(database=database),
      result="row"
   )
   ## Get parents ----
   cql <- c(
      sprintf(
         'MATCH (c:%s)-[r:is_a {origin:$database}]->(p:%s)',
         concept, concept
      ),
      'RETURN DISTINCT c.name AS child,  p.name AS parent'
   )
   parents <- call_dodo(
      neo2R::cypher,
      prepCql(cql),
      parameters=list(database=database),
      result="row"
   )
   stopifnot(
      all(parents$child %in% concepts$name),
      all(parents$parent %in% concepts$name)
   )
   ## Get alternative concepts ----
   cql <- c(
      sprintf(
         'MATCH (c:%s)-[:is_in]->(:Database {name:$database})',
         concept
      ),
      sprintf(
         'MATCH (a:%s)-[:is_in]->(:Database {name:$database})',
         concept
      ),
      'MATCH (a)-[r:is_alt]->(c)',
      'RETURN DISTINCT a.name as alt, c.name as name'
   )
   alt <- call_dodo(
      neo2R::cypher,
      prepCql(cql),
      parameters=list(database=database),
      result="row"
   )
   stopifnot(
      all(alt$name %in% concepts$name),
      all(alt$alt %in% concepts$name)
   )
   ## Return all results ----
   return(list(
      concepts=concepts,
      parents=parents,
      alt=alt
   ))
}

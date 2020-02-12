#' Embryonic function to get full ontology
#' 
#' @export
#' 
get_ontology <- function(database, concept=c("Disease", "Phenotype")){
   concept <- match.arg(concept)
   ## Get parents ----
   cql <- c(
      sprintf(
         'MATCH (c:%s)-[r:is_a {origin:$database}]->(p:%s)',
         concept, concept
      )
   )
   parents <- call_dodo(
      neo2R::cypher,
      prepCql(c(
         cql,
         'RETURN c.name AS child,  p.name AS parent'
      )),
      parameters=list(database=database),
      result="row"
   )
   concepts <- call_dodo(
      neo2R::cypher,
      prepCql(c(
         cql,
         'WITH collect(c)+collect(p) AS cc',
         'UNWIND cc AS c',
         'RETURN c.name AS name, c.shortID AS shortID, c.label AS label, ',
         'c.definition AS definition, c.level AS level'
      )),
      parameters=list(database=database),
      result="row"
   )
   ## Get other concepts ----
   cql <- c(
      sprintf(
         'MATCH (c:%s)-[:is_in]->(:Database {name:$database})',
         concept
      ),
      'WHERE NOT (c)-[:is_a {origin:$database}]->()'
   )
   concepts2 <- call_dodo(
      neo2R::cypher,
      prepCql(c(
         cql,
         'RETURN c.name AS name, c.shortID AS shortID, c.label AS label, ',
         'c.definition AS definition, c.level AS level'
      )),
      parameters=list(database=database),
      result="row"
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
      'MATCH (a)-[r:is_alt]->(c)'
   )
   alt <- call_dodo(
      neo2R::cypher,
      prepCql(c(
         cql,
         'RETURN a.name as alt, c.name as name'
      )),
      parameters=list(database=database),
      result="row"
   )
   ## Return all results ----
   return(list(
      concepts=unique(rbind(concepts, concepts2)),
      parents=parents,
      alt=alt
   ))
}

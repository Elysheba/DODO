#' Embryonic function to convert id
#' 
#' @export
#' 
convert_id <- function(from, concept=c("Disease", "Phenotype")){
   concept <- match.arg(concept)
   from <- setdiff(from, NA)
   stopifnot(is.character(from), length(from)>0)
   ##
   cql <- c(
      sprintf(
         'MATCH (f:%s) WHERE f.name IN $from',
         concept
      ),
      'CALL apoc.path.expandConfig(',
      'f, {uniqueness:"NODE_GLOBAL", relationshipFilter:"is_xref_nba>"}',
      ') YIELD path',
      # 'WITH (nodes(path))[0] as s, last(nodes(path)) as e',
      # 'MATCH (e)-[:is_related|is_xref*0..1]->(e2)'
      'RETURN distinct',
      '(nodes(path))[0].name AS from, ',
      'last(nodes(path)).name AS to, ',
      'length(relationships(path)) AS hops'
   )
   toRet <- call_dodo(
      neo2R::cypher,
      prepCql(cql),
      parameters=list(from=from),
      result="row"
   )
   return(toRet)
}

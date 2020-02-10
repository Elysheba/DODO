###############################################################################@
#' Feeding DODO: load concept synonyms in DODO DB
#'
#' Not exported to avoid unintended modifications of the DB.
#'
#' @param toImport a data.frame with the following columns:
#' - database: *character* name of the database (mandatory)
#' - shortID: *character* short concept ID (mandatory)
#' - value: *character* concept synonym (mandatory)
#' @param concept either "Disease" or "Phenotype"
#'
load_concept_synonyms <- function(toImport, concept){
   concept <- match.arg(concept, c("Disease", "Phenotype"))
   ## Checks ----
   tlc <- c(
      "database"="character",
      "shortID"="character",
      "value"="character"
   )
   mandatory <- c(
      "database",
      "shortID",
      "value"
   )
   check_df_to_import(toImport, tlc, mandatory)
   toImport$name <- paste(toImport$database, toImport$shortID, sep=":")
   toImport$value_up <- toupper(toImport$value)
   toImport <- toImport[, c("name", "value", "value_up")]
   
   ## Query ----
   cql <- c(
      sprintf('MATCH (c:%s {name:row.name})', concept),
      'MERGE (s:Synonym {value:row.value, value_up:row.value_up})',
      'MERGE (c)-[:is_known_as]->(s)'
   )
   import_in_dodo(neo2R::prepCql(cql), toImport)
}

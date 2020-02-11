###############################################################################@
#' Feeding DODO: load concept alternative identifiers in DODO DB
#'
#' Not exported to avoid unintended modifications of the DB.
#'
#' @param toImport a data.frame with the following columns:
#' - database: *character* name of the database (mandatory)
#' - shortID: *character* short concept ID (mandatory)
#' - altdb: *character* name of the database for
#' alternative identifiers (mandatory)
#' - altid: *character* alternative short concept identifiers (mandatory)
#' @param concept either "Disease" or "Phenotype"
#'
load_alternative_identifiers <- function(toImport, concept){
   ## Checks ----
   concept <- match.arg(concept, c("Disease", "Phenotype"))
   tlc <- c(
      "database"="character",
      "shortID"="character",
      "altdb"="character",
      "altid"="character"
   )
   mandatory <- c(
      "database",
      "shortID",
      "altdb",
      "altid"
   )
   check_df_to_import(toImport, tlc, mandatory)
   toImport$name <- paste(toImport$database, toImport$shortID, sep=":")
   toImport$alt <- paste(toImport$altdb, toImport$altid, sep=":")
   
   ## Concepts ----
   cToImport1 <- toImport[, c("database", "shortID")]
   cToImport2 <- toImport[, c("altdb", "altid")]
   colnames(cToImport1) <- colnames(cToImport2) <- c("database", "shortID")
   cToImport <- unique(rbind(cToImport1, cToImport2))
   load_concept_names(cToImport, concept)
   
   ## Query ----
   cql <- c(
      sprintf('MATCH (c:%s {name:row.name})', concept),
      sprintf('MATCH (a:%s {name:row.alt})', concept),
      'MERGE (a)-[:is_alt]->(c)'
   )
   import_in_dodo(neo2R::prepCql(cql), toImport[, c("name", "alt")])
}

###############################################################################@
#' Feeding DODO: load concept parent identifiers in DODO DB
#'
#' Not exported to avoid unintended modifications of the DB.
#'
#' @param toImport a data.frame with the following columns:
#' - database: *character* name of the database (mandatory)
#' - shortID: *character* short concept ID (mandatory)
#' - parentdb: *character* name of the database for
#' parent identifiers (mandatory)
#' - parentid: *character* parent short concept identifiers (mandatory)
#' @param concept either "Disease" or "Phenotype"
#'
load_parent_identifiers <- function(toImport, concept, origin){
   ## Checks ----
   concept <- match.arg(concept, c("Disease", "Phenotype"))
   stopifnot(is.character(origin), !is.na(origin), length(origin)==1)
   tlc <- c(
      "database"="character",
      "shortID"="character",
      "parentdb"="character",
      "parentid"="character"
   )
   mandatory <- c(
      "database",
      "shortID",
      "parentdb",
      "parentid"
   )
   check_df_to_import(toImport, tlc, mandatory)
   toImport$name <- paste(toImport$database, toImport$shortID, sep=":")
   toImport$parent <- paste(toImport$parentdb, toImport$parentid, sep=":")
   
   ## Concepts ----
   cToImport1 <- toImport[, c("database", "shortID")]
   cToImport2 <- toImport[, c("parentdb", "parentid")]
   colnames(cToImport1) <- colnames(cToImport2) <- c("database", "shortID")
   cToImport <- unique(rbind(cToImport1, cToImport2))
   load_concept_names(cToImport, concept)

   ## Query ----
   cql <- c(
      sprintf('MATCH (c:%s {name:row.name})', concept),
      sprintf('MATCH (p:%s {name:row.parent})', concept),
      sprintf('MERGE (c)-[:is_a {origin:"%s"}]->(p)', origin)
   )
   import_in_dodo(neo2R::prepCql(cql), toImport[, c("name", "parent")])
}

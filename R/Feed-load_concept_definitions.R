###############################################################################@
#' Feeding DODO: load concept defintions in DODO DB
#'
#' Not exported to avoid unintended modifications of the DB.
#'
#' @param toImport a data.frame with the following columns:
#' - database: *character* name of the database (mandatory)
#' - shortID: *character* short concept ID (mandatory)
#' - label: *character* concept label (optional)
#' - definition: *character* concept definition (optional)
#' - level: *integer* level in database ontology (optional)
#' @param concept either "Disease" or "Phenotype"
#'
load_concept_definitions <- function(toImport, concept){
   concept <- match.arg(concept, c("Disease", "Phenotype"))
   ## Checks ----
   tlc <- c(
      "database"="character",
      "shortID"="character",
      "label"="character",
      "definition"="character",
      "level"="integer"
   )
   mandatory <- c(
      "database",
      "shortID"
   )
   check_df_to_import(toImport, tlc, mandatory)
   toImport$name <- paste(toImport$database, toImport$shortID, sep=":")
   toImport$label_up <- toupper(toImport$label)
   toImport$defintion_up <- toupper(toImport$definition)
   
   ## Load databases first ----
   dbs <- unique(toImport[, "database", drop=FALSE])
   colnames(dbs) <- "name"
   load_db_names(dbs)
   
   ## Query ----
   cql <- c(
      'MATCH (db:Database {name:row.database})',
      sprintf('MERGE (c:%s {name:row.name})-[:is_in]->(db)', concept),
      'SET c.shortID=row.shortID, ',
      'c.label=row.label, c.label_up=row.label_up, ',
      'c.definition=row.definition, c.definition_up=row.definition_up, ',
      'c.level=toInteger(row.level)'
   )
   import_in_dodo(neo2R::prepCql(cql), toImport)
}

###############################################################################@
#' Feeding DODO: load concept names in DODO DB
#'
#' Not exported to avoid unintended modifications of the DB.
#'
#' @param toImport a data.frame with the following columns:
#' - database: *character* name of the database (mandatory)
#' - shortID: *character* short concept ID (mandatory)
#'
load_concept_names <- function(toImport, concept){
   concept <- match.arg(concept, c("Disease", "Phenotype"))
   ## Checks ----
   tlc <- c(
      "database"="character",
      "shortID"="character"
   )
   mandatory <- c(
      "database",
      "shortID"
   )
   check_df_to_import(toImport, tlc, mandatory)
   toImport$name <- paste(toImport$database, toImport$shortID, sep=":")
   
   ## Load databases first ----
   dbs <- unique(toImport[, "database", drop=FALSE])
   colnames(dbs) <- "name"
   load_db_names(dbs)
   
   ## Query ----
   cql <- c(
      'MATCH (db:Database {name:row.database})',
      sprintf('MERGE (c:%s {name:row.name})-[:is_in]->(db)', concept),
      'SET c.shortID=row.shortID'
   )
   import_in_dodo(neo2R::prepCql(cql), toImport)
}

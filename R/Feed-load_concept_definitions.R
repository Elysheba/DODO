###############################################################################@
#' Feeding DODO: load concept defintions in DODO DB
#'
#' Not exported to avoid unintended modifications of the DB.
#'
#' @param toImport a data.frame with the following columns:
#' - database: *character* name of the concept reference database (mandatory)
#' - origin: *character* name of the database from which the concept was
#' taken (optional). If not provided or NA, the database field is used.
#' - shortID: *character* short concept ID (mandatory)
#' - label: *character* concept label (optional)
#' - definition: *character* concept definition (optional)
#' - level: *integer* level in database ontology (optional)
#' @param concept either "Disease" or "Phenotype"
#'
load_concept_definitions <- function(toImport, concept){
   concept <- match.arg(concept, c("Disease", "Phenotype"))
   if(!"origin" %in% colnames(toImport)){
      toImport$origin <- toImport$database
   }
   ## Checks ----
   tlc <- c(
      "database"="character",
      "origin"="character",
      "shortID"="character",
      "label"="character",
      "definition"="character",
      "level"="integer"
   )
   mandatory <- c(
      "database",
      "shortID"
   )
   neoDODO:::check_df_to_import(toImport, tlc, mandatory)
   toImport$name <- paste(toImport$database, toImport$shortID, sep=":")
   toImport$origin <- ifelse(
      is.na(toImport$origin), toImport$database, toImport$origin
   )
   toImport$label_up <- toupper(toImport$label)
   toImport$defintion_up <- toupper(toImport$definition)
   
   ## Load databases first ----
   dbs <- unique(toImport[, "origin", drop=FALSE])
   colnames(dbs) <- "name"
   neoDODO:::load_db_names(dbs)
   
   ## Query ----
   cql <- c(
      'MATCH (db:Database {name:row.origin})',
      sprintf('MERGE (c:%s {name:row.name})', concept),
      'SET c.shortID=row.shortID, ',
      'c.label=row.label, c.label_up=row.label_up, ',
      'c.definition=row.definition, c.definition_up=row.definition_up, ',
      'c.level=toInteger(row.level)',
      'MERGE (c)-[:is_in]->(db)'
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
#' - origin: *character* name of the database from which the concept was
#' taken (optional). If not provided or NA, the database field is used.
#' - shortID: *character* short concept ID (mandatory)
#'
load_concept_names <- function(toImport, concept){
   concept <- match.arg(concept, c("Disease", "Phenotype"))
   if(!"origin" %in% colnames(toImport)){
      toImport$origin <- toImport$database
   }
   ## Checks ----
   tlc <- c(
      "database"="character",
      "origin"="character",
      "shortID"="character"
   )
   mandatory <- c(
      "database",
      "shortID"
   )
   check_df_to_import(toImport, tlc, mandatory)
   toImport$name <- paste(toImport$database, toImport$shortID, sep=":")
   toImport$origin <- ifelse(
      is.na(toImport$origin), toImport$database, toImport$origin
   )
   
   ## Load databases first ----
   dbs <- unique(toImport[, "origin", drop=FALSE])
   colnames(dbs) <- "name"
   load_db_names(dbs)
   
   ## Query ----
   cql <- c(
      'MATCH (db:Database {name:row.origin})',
      sprintf('MERGE (c:%s {name:row.name})', concept),
      'SET c.shortID=row.shortID',
      'MERGE (c)-[:is_in]->(db)'
   )
   import_in_dodo(neo2R::prepCql(cql), toImport)
}

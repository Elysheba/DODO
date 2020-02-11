###############################################################################@
#' Feeding DODO: load diseases-phenotypes relationships in DODO DB
#'
#' Not exported to avoid unintended modifications of the DB.
#'
#' @param toImport a data.frame with the following columns:
#' - diseaseDB: *character* name of the disease database (mandatory)
#' - diseaseID: *character* short disease ID (mandatory)
#' - phenoDB: *character* name of the phenotype database (mandatory)
#' - phenoID: *character* short phenotype ID (mandatory)
#'
load_has_phenotypes <- function(toImport){
   ## Checks ----
   tlc <- c(
      "diseaseDB"="character",
      "diseaseID"="character",
      "phenoDB"="character",
      "phenoID"="character"
   )
   mandatory <- c(
      "diseaseDB",
      "diseaseID",
      "phenoDB",
      "phenoID"
   )
   check_df_to_import(toImport, tlc, mandatory)
   toImport$disease <- paste(toImport$diseaseDB, toImport$diseaseID, sep=":")
   toImport$phenotype <- paste(toImport$phenoDB, toImport$phenoID, sep=":")
   
   ## Concepts ----
   diseases <- toImport[, c("diseaseDB", "diseaseID")]
   phenotypes <- toImport[, c("phenoDB", "phenoID")]
   colnames(diseases) <- colnames(phenotypes) <- c("database", "shortID")
   load_concept_names(diseases, "Disease")
   load_concept_names(phenotypes, "Phenotype")
   
   ## Query ----
   cql <- c(
      'MATCH (d:Disease {name:row.disease})',
      'MATCH (p:Phenotype {name:row.phenotype})',
      'MERGE (d)-[:has_pheno]->(p)'
   )
   import_in_dodo(neo2R::prepCql(cql), toImport[, c("disease", "phenotype")])
}

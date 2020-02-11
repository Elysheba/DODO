###############################################################################@
#' Feeding DODO: load concept cross references in DODO DB
#'
#' Not exported to avoid unintended modifications of the DB.
#'
#' @param toImport the data frame of cross references with the following column
#' - DB1: *character* name of the database 1 (mandatory)
#' - id1: *character* short concept ID 1 (mandatory)
#' - DB2: *character* name of the database 2 (mandatory)
#' - id2: *character* short concept ID 2 (mandatory)
#' @param xrefDB a data frame with 2 columns ("DB1", "DB2") indicating couples
#' of DB considered close enough to define "xref". Identifiers from other
#' couples of DB will be considered as being "related"
#' @param concept either "Disease" or "Phenotype"
#'
load_cross_references <- function(toImport, xrefDB, concept){
   ## Checks ----
   concept <- match.arg(concept, c("Disease", "Phenotype"))
   tlc <- c(
      "DB1"="character",
      "id1"="character",
      "DB2"="character",
      "id2"="character"
   )
   mandatory <- c(
      "DB1",
      "id1",
      "DB2",
      "id2"
   )
   check_df_to_import(toImport, tlc, mandatory)
   tlc <- c(
      "DB1"="character",
      "DB2"="character"
   )
   mandatory <- c(
      "DB1",
      "DB2"
   )
   check_df_to_import(xrefDB, tlc, mandatory)
   
   ## Split cross references ----
   # xdb <- apply(xrefDB, 1, function(x) paste(sort(x), collapse=".."))
   xdb <- paste(
      pmin(xrefDB$DB1, xrefDB$DB2), pmax(xrefDB$DB1, xrefDB$DB2),
      sep=".."
   )
   ddb <- paste(
      pmin(toImport$DB1, toImport$DB2), pmax(toImport$DB1, toImport$DB2),
      sep=".."
   )
   sToImport <- list(
      'is_xref'=toImport[which(ddb %in% xdb),],
      'is_related'=toImport[which(!ddb %in% xdb),]
   )
   
   ## Import function ----
   import <- function(type){
      toImport <- sToImport[[type]]
      
      ## Concepts ----
      cToImport1 <- toImport[, c("DB1", "id1")]
      cToImport2 <- toImport[, c("DB2", "id2")]
      colnames(cToImport1) <- colnames(cToImport2) <- c("database", "shortID")
      cToImport <- unique(rbind(cToImport1, cToImport2))
      load_concept_names(cToImport, concept)
      
      ## References ----
      toImport$f <- paste(toImport$DB1, toImport$id1, sep=":")
      toImport$t <- paste(toImport$DB2, toImport$id2, sep=":")
      cql <- c(
         sprintf('MATCH (f:%s {name:row.f})', concept),
         sprintf('MATCH (t:%s {name:row.t})', concept),
         sprintf('MERGE (f)-[:%s]->(t)', type),
         sprintf('MERGE (f)<-[:%s]-(t)', type)
      )
      import_in_dodo(neo2R::prepCql(cql), toImport[,c("f", "t")])
      
      ## Update ambiguity ----
      call_dodo(neo2R::cypher, query=prepCql(c(
         sprintf('MATCH (c)-[f:%s]->(r)-[b:%s]->(c)', type, type),
         'MATCH (r)-[:is_in]->(d:Database)',
         'WITH c.name AS cname, d.name AS refDB,',
         'count(r) AS rcount,',
         'collect(f) AS allf, collect(b) AS allb',
         'FOREACH(e in allf | set e.FA=rcount)',
         'FOREACH(e in allb | set e.BA=rcount)'
      )))
      call_dodo(
         neo2R::cypher,
         query=prepCql(c(
            sprintf(
               'MATCH (f:%s)-[:%s {BA:1}]->(t:%s)',
               concept, type, concept
            ),
            sprintf('MERGE (f)-[:%s]->(t)', paste0(type, "_nba"))
         ))
      )
      call_dodo(
         neo2R::cypher,
         query=prepCql(c(
            sprintf(
               'MATCH (f:%s)-[r:%s]->(t:%s) WHERE r.BA > 1',
               concept, type, concept
            ),
            sprintf('MATCH (f)-[nba:%s]->(t)', paste0(type, "_nba")),
            'DELETE nba'
         ))
      )
      
   }
   
   ## xref ----
   import("is_xref")
   
   ## related ----
   import("is_related")
   
}


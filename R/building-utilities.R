###############################################################################@
#' Check data frame to import in DODO
#' 
#' Feeding helper function not exported
#' 
#' @param toImport the object to check
#' @param tlc a named vector of type per column name
#' @param mandatory a vector of mandatory columns
#' 
#' @details Fails if conditions are not fulfilled
#' 
check_df_to_import <- function(toImport, tlc, mandatory){
  if(!is.data.frame(toImport)) stop("toImport must be a data.frame")
  if(!identical(sort(names(tlc)), sort(colnames(toImport)))){
    stop("toImport columns must be: ", paste(names(tlc), collapse=", "))
  }
  for(cn in names(tlc)){
    values <- toImport[, cn, drop=TRUE]
    if(!is(values, tlc[cn])){
      stop(sprintf("%s should be a %s", cn, tlc[cn]))
    }
    if(cn %in% mandatory && any(is.na(values))){
      stop(sprintf("%s is mandatory ==> must not contain any NA", cn))
    }
  }
}


###############################################################################@
#' Feeding DODO: Set the DODO version
#'
#' Not exported to avoid unintended modifications of the DB.
#' This function is used when modifying the DODO content.
#'
#' @param dodoInstance instance of DODO to be set
#' @param dodoVersion version of DODO to be set
#'
set_dodo_version <- function(dodoInstance, dodoVersion){
  call_dodo(
    neo2R::cypher,
    query=neo2R::prepCql(c(
      'MERGE (n:System {name:"DODO"})',
      sprintf(
        'SET n.instance = "%s"',
        dodoInstance
      ),
      sprintf(
        'SET n.version = "%s"',
        dodoVersion
      )
    ))
  )
}


###############################################################################@
#' Feeding DODO: Load DODO data model in neo4j
#'
#' Not exported to avoid unintended modifications of the DB.
#'
load_dodo_model <- function(){
  pkgname <- utils::packageName()
  ## Model
  cqlFile <- system.file(
    "documentation", "data-model", "DODO.cql",
    package=pkgname
  )
  queries <- neo2R::readCql(cqlFile)
  for(query in queries){
    call_dodo(neo2R::cypher, query=query)
  }
  ##
  invisible(TRUE)
}


###############################################################################@
#' Feeding DODO: Register databases in DODO DB
#'
#' Not exported to avoid unintended modifications of the DB.
#'
#' @param toImport a data.frame with the following columns:
#' - name: *character* name of the database (mandatory)
#'
load_db_names <- function(toImport){
  ## Checks ----
  tlc <- c("name"="character")
  mandatory <- c("name")
  neoDODO:::check_df_to_import(toImport, tlc, mandatory)
  
  ## Query ----
  cql <- c(
    'MERGE (db:Database {name:row.name})'
  )
  neoDODO:::import_in_dodo(cql = neo2R::prepCql(cql), toImport = toImport)
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
  neoDODO:::load_db_names(toImport = dbs)
  
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
  toImport <- toImport
  
  ## Concepts ----
  cToImport <- unique(toImport[, c("database", "shortID")])
  load_concept_names(cToImport, concept)
  
  ## Query ----
  cql <- c(
    sprintf('MATCH (c:%s {name:row.name})', concept),
    'MERGE (s:Synonym {value:row.value, value_up:row.value_up})',
    'MERGE (c)-[:is_known_as]->(s)'
  )
  import_in_dodo(
    neo2R::prepCql(cql),
    toImport[, c("name", "value", "value_up")]
  )
}

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

###############################################################################@
#' Feeding DODO: Register databases with URL template in DODO DB
#'
#' Not exported to avoid unintended modifications of the DB.
#'
#' @param toImport a data.frame with the following columns:
#' - name: *character* name of the database (mandatory)
#' - idURL: *character* URL template for database concepts (optional)
#'
load_db_definitions <- function(toImport){
  ## Checks ----
  tlc <- c("name"="character", "idURL"="character")
  mandatory <- c("name")#, "idURL")
  neoDODO:::check_df_to_import(toImport, tlc, mandatory)
  
  ## Query ----
  cql <- c(
    'MERGE (db:Database {name:row.name})',
    'SET db.idURL=row.idURL'
  )
  import_in_dodo(neo2R::prepCql(cql), toImport)
}

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



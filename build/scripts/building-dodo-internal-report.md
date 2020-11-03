---
title: "Building DODO database"
author: "Liesbeth François"
date: "October 27 2020"
abstract: "This report records the process the load the different disease ontologies into a single DODO instance and return an RDF table as well as RData objects with all parsed information."
output:
    html_document:
        fig_width: 9
        fig_height: 5
        keep_md: yes
        number_sections: yes
        theme: cerulean
        toc: yes
        toc_float: yes
params:
  type: "internal"
  name: "dodo"
editor_options: 
  chunk_output_type: console
---

<!----------------------------------------------------------------->
<!----------------------------------------------------------------->

# Introduction

This document shows how to build a Dictionary of Disease Ontologies (DODO). It can be adapted for specific needs or additional ontologies as required. The DODO functions used to feed the DB are not exported to avoid unintended modifications of the DB. To call them, they are preceded by `DODO:::`.

In this example several publically available disease ontologies are integrated into DODO. These ontologies have been parse beforehand and these scripts are made available through a GitHub repository (as described in the paper).


```r
##
library(DODO)
library(knitr)
library(here)
library(dplyr)
library(readr)
library(tibble)
library(stringr)
library(DT)
library(ReDaMoR)
##

workingDirectory <- here("build/working")
# setwd("~/Documents/Development/DODO/build/working/")
##
# opts_knit$set(root.dir=workingDirectory)
opts_chunk$set(
   eval=TRUE,
   message=FALSE,
   root.dir=workingDirectory
)
## Specific config
if(params$type == "internal"){
  dodoInstance <- "UCB-Internal"
}else{
  dodoInstance <- "UCB-Public"
}
dodoVersion <- format(Sys.Date(), "%Y.%m.%d")
## General config
reDumpThr <- as.difftime(2, units="days")
curDate <- Sys.Date()
## Original resources location
# oriDir <- "~/Shared/Data-Science/Data-Source-Model-Repository"
## Connect to DODO database
# if(params$type == "internal"){
#   url <- "http://localhost:7475"
# }else{
#   url <- "http://localhost:7476"
# }

## tmp docker
url <- "http://localhost:7470"
connect_to_dodo(
   url = url,
   importPath = "/data/lfrancois/Development/DODO/build/working/neo4jImport"
   # importPath = "neo4jImport"
)
```

```
## Warning in check_dodo_connection(verbose = TRUE): DODO DB is empty !
```

```
## Warning in check_dodo_connection(): DODO DB is empty !

## Warning in check_dodo_connection(): DODO DB is empty !
```

```
## Warning in check_dodo_cache(newCon = TRUE): Clearing cache
```

```
## Warning in check_dodo_connection(verbose = FALSE): DODO DB is empty !
```

<!----------------------------------------------------------------->
<!----------------------------------------------------------------->

# Data model

!["The DODO data model"](/data/lfrancois/Development/DODO/inst/documentation/data-model/DODO.svg)

# Load ontologies

The different ontologies included are MonDO, EFO, DO, Orphanet, MedGen, MeSH, HPO, ICD10CM, ICD11 (only ICD10 ids), Cortellis (if available) and ICD10CM. These files were parsed with the source scripts provided in the respective GitHub repositories. 


```r
path_name <- "/home/lfrancois/Shared/Data-Science/Data-Source-Model-Repository"
src <- c("Monarch","EFO","DO","Orphanet","MedGen","MeSH","HPO", "ICD11", "CortellisONT","ICD10CM")
for(s in src){
  ddir <- file.path(path_name,s,"data")
  dbfiles <- grep("txt",list.files(ddir),value =T)
  for(i in dbfiles){
    x <- read.table(file.path(ddir,i),header = TRUE, sep = "\t", 
                    quote = '"', comment.char = "", 
                    colClasses= c("character"))
    
    if(grepl("entryId",i)){
      if(grepl("OMIM", i)){
        x$origin <- "OMIM"
      }else if(grepl("ICD10_", i)){
        x$origin <- "ICD10"
      }else{
        x$origin <- s
      }
    }
    assign(gsub(".txt","",i), x) 
  }
}

## Some issues in the first lines of HPO files
# toRem <- grep("disease-db", HPO_diseaseHP$db)
# HPO_diseaseHP <- HPO_diseaseHP[-toRem,]
# toRem <- grep("disease-db", HPO_diseases$db)
# HPO_diseases <- HPO_diseases[-toRem,]
# toRem <- grep("disease-db", HPO_diseaseSynonyms$db)
# HPO_diseaseSynonyms <- HPO_diseaseSynonyms[-toRem,]
```


# Format input tables

This script generates the different tables required to build the DODO Dgraph database and takes the files created by parsing the disease ontologies as input. The ontologies provided in public DODO instance are:

- MONDO (Monarch)
- EFO
- DO
- Orphanet
- MedGen
- MeSH
- HPO
- ICD11
- ICD10CM
- ClinVar

The URL provides a link to its respective GitHub repository where you can find the scripts to parse the different ontologies and generate the necessary input files for the current workflow. However, this workflow is easily adapted to include additional disease or phenotype ontologies. It requires parsing the ontology and generating files with specific format and specific table name. The table name is always constructes as follows: *name of the ontology*_*name table*, eg. "Monarch_entryId". The script will read these files with the specific table names from the global environment and append them automatically to each other. The format of each table is listed below, if information is not available or not provided in the ontology, the table doesn't need to be created. If specific information of a table is missing, please assign NA to that column. 

**entryId**

Dataframe with columns *DB* (character, database or ontology), *id* (character, identifier without database prefix), *def* (character, definition of the concept), and *level* (integer, in the ontology hierarchy).

<!--html_preserve--><div id="htmlwidget-45ff0f1c83750272d11b" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-45ff0f1c83750272d11b">{"x":{"filter":"none","data":[["1","2"],["MONDO","MONDO"],["0002974","0002975"],["A primary or metastatic malignant neoplasm involving the cervix.","A melanoma that arises usually from the breast skin and less often from the breast glandular tissue. Primary breast melanomas are rare."],["9","7"],["Monarch","Monarch"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>DB<\/th>\n      <th>id<\/th>\n      <th>def<\/th>\n      <th>level<\/th>\n      <th>origin<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

**idNames**

Dataframe with columns *DB* (character, database or ontology), *id* (character, identifier without database prefix), *syn* (character, synonym and label), and canonical (boolean, whether *syn* is the canonical concept label).

<!--html_preserve--><div id="htmlwidget-d65e288b6b6600831450" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-d65e288b6b6600831450">{"x":{"filter":"none","data":[["1","2"],["MONDO","MONDO"],["0002974","0002975"],["cervical cancer","malignant breast melanoma"],["TRUE","TRUE"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>DB<\/th>\n      <th>id<\/th>\n      <th>syn<\/th>\n      <th>canonical<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

**parentId**

Dataframe with columns *DB* (character, database or ontology), *id* (character, identifier without the database prefex), *pDB* (character, database or ontology of the parent term), *parent* (character, identifiers of the parent), origin (character, database or ontology where this relationship originates from).

<!--html_preserve--><div id="htmlwidget-9ce08dac355abc8bd114" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-9ce08dac355abc8bd114">{"x":{"filter":"none","data":[["1","2"],["MONDO","MONDO"],["0016629","0014497"],["MONDO","MONDO"],["0002243","0019052"],["MONDO","MONDO"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>DB<\/th>\n      <th>id<\/th>\n      <th>pDB<\/th>\n      <th>parent<\/th>\n      <th>origin<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

**crossId**

Dataframe with columns *DB1* (character, database or ontology of the first identifier), *id1* (character, first identifier without the database prefex), *DB2* (character, database or ontology of the second identifier), *id2* (character, second identifier also called cross-referenced identifier without database prefix).

<!--html_preserve--><div id="htmlwidget-5fb4ca1b02e448b9fcd6" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-5fb4ca1b02e448b9fcd6">{"x":{"filter":"none","data":[["1","2"],["MONDO","MONDO"],["0002974","0002974"],["ICD9","OMIM"],["180.8","603956"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>DB1<\/th>\n      <th>id1<\/th>\n      <th>DB2<\/th>\n      <th>id2<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

**altId**

Dataframe with columns *DB* (character, database or ontology), *id* (character, identifier without the database prefex), *alt* (character, database or ontology of the alternative identifier), *altDB* (character, alternative identifier).

<!--html_preserve--><div id="htmlwidget-c37b3ac979d1f4a9a358" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-c37b3ac979d1f4a9a358">{"x":{"filter":"none","data":[["1","2"],["0000003","0000005"],["0004715","0001453"],["HP","HP"],["HP","HP"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>id<\/th>\n      <th>alt<\/th>\n      <th>altDB<\/th>\n      <th>DB<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

**pheno2dis**

Dataframe with columns *phenoDB* (character, database or ontology of the phenotype), *phenoID* (character, phenotype identifier without the database prefex), *disDB* (character, database or ontology of the disease identifier), *disID* (character, disease identifier).

<!--html_preserve--><div id="htmlwidget-e389891d72e122395b83" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-e389891d72e122395b83">{"x":{"filter":"none","data":[["1","2"],["HP","HP"],["0000252","0001249"],["DECIPHER","DECIPHER"],["1","1"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>phenoDB<\/th>\n      <th>phenoID<\/th>\n      <th>disDB<\/th>\n      <th>disID<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

## Database convention

There are often different abbreviations available for different ontology database. Here, we adopt a naming convention to harmonize the different inputs.

<!--html_preserve--><div id="htmlwidget-5884dfd53f43e9e6c7e9" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-5884dfd53f43e9e6c7e9">{"x":{"filter":"none","data":[["MeSH","MONDO","EFO","OMIMPS","OMIM","Orphanet","SNOMEDCT","SCTID","DOID","NCIt","UMLS","ICD9","ICD10","GARD","MedGen","Human Phenotype Ontology"],["Disease","Disease","Disease","Disease","Disease","Disease","Disease","Disease","Disease","Disease","Disease","Disease","Disease","Disease",null,"Trait"],["MeSH","MONDO","EFO","OMIMPS","OMIM","ORPHA","SNOMEDCT","SNOMEDCT","DOID","NCIt","MedGen","ICD9","ICD10","GARD","MedGen","HP"],["D001368","0000001","0000408","602483","613339 ","377788","702443003","702443003","1","C84570","C0003803","255.4","Q04.3","0007446","C0003803","0001272"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>Database<\/th>\n      <th>Type<\/th>\n      <th>DB<\/th>\n      <th>ID<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

## Cross-reference edges

There are two types of cross-reference edges encoded into the database, *is_xref* and *is_related*.  The *is_xref* edge is used for equal cross-reference relationships where the concepts relate more directly to each other (similar concept levels). The *is_related* edge is used for all other cross-reference edges. These edges are defined based on the sum of forward and backward ambiguities between databases. Ontologies with a ambiguity equal or lower than 4 are considered as *is_xref* with the exception of ICD10 and ICD9 which are never an *is_xref* edge except between these two databases. In addition, MedGen and UMLS identifiers are duplicated therefore there is an additional *is_xref* edge between these. For more information please consult the vignette. 

<!--html_preserve--><div id="htmlwidget-aca0ce484a7d2b16f90e" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-aca0ce484a7d2b16f90e">{"x":{"filter":"none","data":[["EFO","MedGen","OPRHA","ORPHA","Cortellis_condition","ORPHA","ORPHA","ORPHA","DOID","EFO","ORPHA","ICD10","Cortellis_indication","ORPHA","ClinVar","Cortellis_indication","MONDO","ORPHA","Cortellis_condition","EFO","DOID","OMIM","MedGen","UMLS","Cortellis_condition","Cortellis_indication","DOID","EFO","ORPHA","ClinVar","MedGen","MONDO","ORPHA","UMLS","ClinVar","DOID","EFO","MedGen","MONDO","ORPHA","UMLS","ClinVar","DOID","MONDO","ORPHA","ClinVar","MedGen","ORPHA"],["ClinVar","ClinVar","ClinVar","Cortellis_condition","Cortellis_indication","Cortellis_indication","DOID","EFO","GARD","GARD","GARD","ICD9","MEDDRA","MedGen","MeSH","MeSH","MeSH","MeSH","MeSH","MeSH","MeSH","MeSH","MeSH","MeSH","MetaBase_disease","MetaBase_disease","MONDO","MONDO","MONDO","NCIt","NCIt","NCIt","NCIt","NCIt","OMIM","OMIM","OMIM","OMIM","OMIM","OMIM","OMIM","SNOMEDCT","SNOMEDCT","SNOMEDCT","SNOMEDCT","UMLS","UMLS","UMLS"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>DB1<\/th>\n      <th>DB2<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

## Harmonize HPO

HPO captures phenotype information, the information in the HPO parsed files needs to be harmonized before joining.


```r
> list2env(DODO:::harmonize_HPO(
+   HPO_diseaseHP,
+   HPO_synonyms,
+   HPO_hp,
+   HPO_altId,
+   HPO_diseases,
+   HPO_parents,
+   Monarch_hp),
+   envir = .GlobalEnv)
```

## Harmonize CortellisONT

Harmonize information from CortellisONT when available.



# Integrate resources

The input tables from the different ontologies are integrated and checked for consistency.


```r
##################################################################@
## Bind all datasets----
obj <- c("entryId","crossId","idNames","parentId","pheno2dis","altId")

for(i in obj){
  dbobj <- grep(i,ls(),value = T)
  print(i)
  sapply(dbobj,
           function(x){
             print(x)
             print(names(get(x)))
           })
  sobj <- unique(do.call(rbind,lapply(dbobj,function(x) get(x))))
  fn <- paste0("DODO_",i)
  assign(value = sobj,x = fn)
}
```

```
## [1] "entryId"
## [1] "CortellisONT_entryId"
## [1] "DB"     "id"     "def"    "level"  "origin"
## [1] "DO_entryId"
## [1] "DB"     "id"     "def"    "level"  "origin"
## [1] "EFO_entryId"
## [1] "DB"     "id"     "def"    "level"  "origin"
## [1] "HPO_entryId"
## [1] "DB"     "id"     "def"    "level"  "origin"
## [1] "ICD10_entryId"
## [1] "DB"     "id"     "def"    "level"  "origin"
## [1] "ICD10CM_entryId"
## [1] "DB"     "id"     "def"    "level"  "origin"
## [1] "MedGen_entryId"
## [1] "DB"     "id"     "def"    "level"  "origin"
## [1] "MedGen_OMIM_entryId"
## [1] "DB"     "id"     "def"    "level"  "origin"
## [1] "Mesh_entryId"
## [1] "DB"     "id"     "def"    "level"  "origin"
## [1] "Monarch_entryId"
## [1] "DB"     "id"     "def"    "level"  "origin"
## [1] "Orphanet_entryId"
## [1] "DB"     "id"     "def"    "level"  "origin"
## [1] "crossId"
## [1] "CortellisONT_crossId"
## [1] "DB1" "id1" "DB2" "id2"
## [1] "DO_crossId"
## [1] "DB1" "id1" "DB2" "id2"
## [1] "EFO_crossId"
## [1] "DB1" "id1" "DB2" "id2"
## [1] "MedGen_crossId"
## [1] "DB1" "id1" "DB2" "id2"
## [1] "Monarch_crossId"
## [1] "DB1" "id1" "DB2" "id2"
## [1] "Orphanet_crossId"
## [1] "DB1" "id1" "DB2" "id2"
## [1] "idNames"
## [1] "CortellisONT_idNames"
## [1] "DB"        "id"        "syn"       "canonical"
## [1] "DO_idNames"
## [1] "DB"        "id"        "syn"       "canonical"
## [1] "EFO_idNames"
## [1] "DB"        "id"        "syn"       "canonical"
## [1] "HPO_idNames"
## [1] "DB"        "id"        "syn"       "canonical"
## [1] "ICD10_idNames"
## [1] "DB"        "id"        "syn"       "canonical"
## [1] "ICD10CM_idNames"
## [1] "DB"        "id"        "syn"       "canonical"
## [1] "MedGen_idNames"
## [1] "DB"        "id"        "syn"       "canonical"
## [1] "MedGen_OMIM_idNames"
## [1] "DB"        "id"        "syn"       "canonical"
## [1] "Mesh_idNames"
## [1] "DB"        "id"        "syn"       "canonical"
## [1] "Monarch_idNames"
## [1] "DB"        "id"        "syn"       "canonical"
## [1] "Orphanet_idNames"
## [1] "DB"        "id"        "syn"       "canonical"
## [1] "parentId"
## [1] "CortellisONT_parentId"
## [1] "DB"     "id"     "pDB"    "parent" "origin"
## [1] "DO_parentId"
## [1] "DB"     "id"     "pDB"    "parent" "origin"
## [1] "EFO_parentId"
## [1] "DB"     "id"     "pDB"    "parent" "origin"
## [1] "HPO_parentId"
## [1] "DB"     "id"     "pDB"    "parent" "origin"
## [1] "ICD10_parentId"
## [1] "DB"     "id"     "pDB"    "parent" "origin"
## [1] "ICD10CM_parentId"
## [1] "DB"     "id"     "pDB"    "parent" "origin"
## [1] "Mesh_parentId"
## [1] "DB"     "id"     "pDB"    "parent" "origin"
## [1] "Monarch_parentId"
## [1] "DB"     "id"     "pDB"    "parent" "origin"
## [1] "Orphanet_parentId"
## [1] "DB"     "id"     "pDB"    "parent" "origin"
## [1] "pheno2dis"
## [1] "HPO_pheno2dis"
## [1] "phenoDB" "phenoID" "disDB"   "disID"  
## [1] "altId"
## [1] "DO_altId"
## [1] "DB"    "id"    "altDB" "alt"  
## [1] "DO_altId~"
## [1] "DB"    "id"    "altDB" "alt"  
## [1] "HPO_altId"
## [1] "id"    "alt"   "altDB" "DB"
```

```r
# table(DODO_crossId$DB1)
# table(DODO_crossId$DB2)
# table(DODO_entryId$DB)
# table(DODO_idNames$DB)
# table(DODO_parentId$DB)
# table(DODO_parentId$pDB)
# table(DODO_pheno2dis$disDB)
# table(DODO_pheno2dis$phenoDB)
# table(DODO_altId$altDB)
# table(DODO_altId$DB)


##################################################################@
## create dbid
DODO_entryId <- DODO_entryId %>%
  distinct() %>%
  mutate(dbid = str_c(DB, id, sep = ":"),
         origin = case_when(origin == "Monarch" ~ "MONDO",
                            origin == "Orphanet" ~ "ORPHA",
                            TRUE ~ origin)) %>%
  mutate(origin = case_when(is.na(origin) ~ DB,
                            TRUE ~ origin))

DODO_pheno2dis <- DODO_pheno2dis %>%
  mutate(ddbid = str_c(disDB, disID, sep = ":"),
         pdbid = str_c(phenoDB, phenoID, sep = ":"))

DODO_crossId <- DODO_crossId %>%
  mutate(dbid1 = str_c(DB1, id1, sep = ":"),
         dbid2 = str_c(DB2, id2, sep = ":"))

DODO_altId <- DODO_altId %>%
  mutate(dbid = str_c(DB, id, sep = ":"),
         adbid = str_c(altDB, alt, sep =":"))

DODO_parentId <- DODO_parentId %>%
  mutate(dbid = str_c(DB, id, sep = ":"),
          pdbid = str_c(pDB, parent, sep = ":"))

DODO_idNames <- DODO_idNames %>%
  mutate(dbid = str_c(DB, id, sep = ":"))
```

## Load ClinVar information

ClinVar uses an internal clinvar disease identifier (t.id) that cross-references to different ontology. To complete the information and facilitate integration with ClinVar resource, this information is added in DODO.


```r
list2env(DODO:::load_ClinVar(
  DODO_entryId = DODO_entryId,
  path_name = "/home/lfrancois/Shared/Data-Science/Data-Source-Model-Repository/ClinVar/data/",
  traitCref = "ClinVar_traitCref.txt", 
  traitName = "ClinVar_traitNames.txt"),
  # MedGen_MGSTY = "/home/lfrancois/Shared/Data-Science/Data-Source-Model-Repository/MedGen/sources/MGSTY.RFF"),
  envir = .GlobalEnv)
```

```
## <environment: R_GlobalEnv>
```

The parsed dataframes are appended to their respective DODO counterparts.


```r
## Add to DODO
DODO_entryId <- rbind(DODO_entryId, cv_entryId) 
DODO_idNames <- rbind(DODO_idNames, cv_idNames)
DODO_crossId <- rbind(DODO_crossId, cv_crossId)

DODO_entryId <- bind_rows(DODO_entryId,
                          DODO_crossId %>% 
                             select(DB = DB2, id = id2) %>% 
                             mutate(def = NA) %>% distinct) %>%
  distinct() %>%
  mutate(dbid = str_c(DB, id, sep = ":"))

DODO_pheno2dis <- bind_rows(DODO_pheno2dis,
                            DODO_crossId %>% 
                              filter(DB2 == "HP") %>% 
                              select(phenoDB = DB2,
                                     phenoID = id2,
                                     disDB = DB1,
                                     disID = id1),
                            DODO_crossId %>% 
                              filter(DB1 == "HP") %>% 
                              select(phenoDB = DB1,
                                     phenoID = id1,
                                     disDB = DB2,
                                     disID = id2)) %>%
  distinct() %>%
  mutate(ddbid = str_c(disDB, disID, sep = ":"),
         pdbid = str_c(phenoDB, phenoID, sep = ":"))

DODO_crossId <- DODO_crossId %>%
  filter(DB2 != "HP",
         DB1 != "HP") %>%
  mutate(dbid1 = str_c(DB1, id1, sep = ":"),
         dbid2 = str_c(DB2, id2, sep = ":"))


## All IDs in DODO_entryId
# table(DODO_pheno2dis$pdbid %in% DODO_entryId$dbid)
# table(DODO_pheno2dis$ddbid %in% DODO_entryId$dbid) ## ok not all ids in entryId -> rm
# table(DODO_altId$dbid %in% DODO_entryId$dbid)
# table(DODO_altId$adbid %in% DODO_entryId$dbid)
# table(DODO_idNames$dbid %in% DODO_entryId$dbid)
# table(DODO_parentId$dbid %in% DODO_entryId$dbid)
# table(DODO_parentId$pdbid %in% DODO_entryId$dbid)
# table(DODO_crossId$dbid1 %in% DODO_entryId$dbid)
# table(DODO_crossId$dbid2 %in% DODO_entryId$dbid)
```

## Medgen Disease concepts only


```r
mg <- read.table("/home/lfrancois/Shared/Data-Science/Data-Source-Model-Repository/MedGen/sources/MGSTY.RFF",
                 sep = "|", 
                 header = TRUE, 
                 comment.char = "", 
                 quote = "",
                 fill = TRUE, 
                 colClasses = c("character"))
## Some ideas have double designation
toKeep <- mg %>% filter(grepl(paste("Disease or Syndrome","Acquired Abnormality",
                                "Anatomical Abnormality","Congenital Abnormality",
                                "Neoplastic Process",
                                sep = "|"), STY)) 
toRem <- mg %>% filter(!X.CUI %in% toKeep$X.CUI)
DODO_crossId <- DODO_crossId %>% 
  filter(!dbid2 %in% c(paste("MedGen", toRem$X.CUI, sep = ":"),
                       paste("UMLS", toRem$X.CUI, sep = ":"))) %>%
  distinct()
dim(DODO_crossId)
```

```
## [1] 369787      6
```

## Modify database name

Some resources use either "MedGen" or "UMLS" as database name for NCI Metathesaurus Links. Identifiers from either will therefore be encoded twice into DODO. Similarly, ICD10CM identifiers are also considered as ICD10 identifiers.



```r
##################################################################@
## Modify DB names and entries ----
## Add UMLS entries as MedGen entries (double entry)
DODO_entryId <- filter(DODO_entryId,DB == "MedGen") %>% 
  mutate(DB =  str_replace_all(DB, "MedGen","UMLS"),
         origin =  str_replace_all(origin, "MedGen","UMLS"),
         dbid = str_replace_all(dbid, "MedGen","UMLS")) %>%
  bind_rows(DODO_entryId) 
tmp <- DODO_entryId %>%
  filter(grepl("MedGen", dbid)) %>%
  mutate(DB1 = DB,
         id1 = id,
         dbid1 = dbid,
         DB2 = "UMLS",
         id2 = id,
         dbid2 = str_replace_all(dbid, "MedGen","UMLS")) %>%
  select(DB1, 
         id1 = id, 
         DB2, 
         id2, 
         dbid1, 
         dbid2)
##
DODO_crossId <- filter(DODO_crossId,(DB1 == "MedGen" | DB2 == "MedGen")) %>% 
  mutate(DB1 =  str_replace_all(DB1, "MedGen","UMLS"),
         dbid1 = str_replace_all(dbid1, "MedGen","UMLS"), 
         DB2 =  str_replace_all(DB2, "MedGen","UMLS"),
         dbid2 = str_replace_all(dbid2, "MedGen","UMLS")) %>%
  bind_rows(DODO_crossId) %>%
  bind_rows(tmp)
##
DODO_idNames <- filter(DODO_idNames,DB == "MedGen") %>% 
  mutate(DB =  str_replace_all(DB, "MedGen","UMLS"),
         dbid = str_replace_all(dbid, "MedGen","UMLS")) %>%
  bind_rows(DODO_idNames)
##
DODO_pheno2dis <- filter(DODO_pheno2dis,disDB == "MedGen") %>% 
  mutate(disDB =  str_replace_all(disDB, "MedGen","UMLS"),
         ddbid = str_replace_all(ddbid, "MedGen","UMLS")) %>%
  bind_rows(DODO_pheno2dis)
##

## Add ICD10CM entries as ICD10 entries (double entry)
DODO_entryId <- filter(DODO_entryId,DB == "ICD10CM") %>% 
  mutate(DB =  str_replace_all(DB, "ICD10CM","ICD10"),
         dbid = str_replace_all(dbid, "ICD10CM","ICD10"),
         origin = str_replace_all(origin, "ICD10CM","ICD10")) %>%
  bind_rows(DODO_entryId) %>%
  distinct()
##
DODO_crossId <- filter(DODO_crossId,(DB1 == "ICD10CM" | DB2 == "ICD10CM")) %>% 
  mutate(DB1 =  str_replace_all(DB1, "ICD10CM","ICD10"),
         dbid1 = str_replace_all(dbid1, "ICD10CM","ICD10"), 
         DB2 =  str_replace_all(DB2, "ICD10CM","ICD10"),
         dbid2 = str_replace_all(dbid2, "ICD10CM","ICD10")) %>%
  bind_rows(DODO_crossId) %>%
  distinct()
##
DODO_idNames <- filter(DODO_idNames,DB == "ICD10CM") %>% 
  mutate(DB =  str_replace_all(DB, "ICD10CM","ICD10"),
         dbid = str_replace_all(dbid, "ICD10CM","ICD10")) %>%
  bind_rows(DODO_idNames) %>%
  distinct(dbid, syn, .keep_all = TRUE)
##
DODO_pheno2dis <- filter(DODO_pheno2dis,disDB == "ICD10CM") %>% 
  mutate(disDB =  str_replace_all(disDB, "ICD10CM","MedGen"),
         ddbid = str_replace_all(ddbid, "ICD10CM","MedGen")) %>%
  bind_rows(DODO_pheno2dis) %>%
  distinct()
##
##
DODO_parentId <- filter(DODO_parentId,pDB == "ICD10CM" | DB == "ICD10CM") %>% 
  mutate(pDB =  str_replace_all(pDB, "ICD10CM","ICD10"),
         pdbid = str_replace_all(pdbid, "ICD10CM","ICD10"),
         DB =  str_replace_all(DB, "ICD10CM","ICD10"),
         dbid = str_replace_all(dbid, "ICD10CM","ICD10"),
         origin = str_replace_all(origin, "ICD10CM","ICD10")) %>%
  bind_rows(DODO_parentId) %>%
  distinct()

## Mistake in DOID parent ID
DODO_parentId <- mutate(DODO_parentId,
                        origin = str_replace_all(origin, "\\bDO\\b","DOID"))
```

## Remove duplicated entries

Duplicated entries are removed from the different DODO tables. 



# Load into Neo4j

The workflow for importing information into the Neo4j instance by assigning all sisease and phenotype identifiers the "Concept" label. First the respective database nodes are created (or matched), where after the information on the nodes, their cross-reference, parent-child, phenotype, and alternative identifier relationships are added. Finally, the label "Phenotype" is added for all phenotype nodes while all others are assigned the "Disease" label.

Connect to Neo4j and specify import path to load data.


```r
connect_to_dodo(
   url=url,
   remember = FALSE,
   importPath = "/data/lfrancois/Development/DODO/build/working/neo4jImport"
   # importPath = "neo4jImport"
)
```

```
## Warning in check_dodo_connection(verbose = TRUE): DODO DB is empty !
```

```
## Warning in check_dodo_connection(): DODO DB is empty !

## Warning in check_dodo_connection(): DODO DB is empty !
```

```
## Warning in check_dodo_cache(newCon = TRUE): Clearing cache
```

```
## Warning in check_dodo_connection(verbose = FALSE): DODO DB is empty !
```

## Check empty DB

Do not go further if your DODO DB is not empty.


```r
dbSize <- call_dodo(neo2R::cypher, 'MATCH (n) RETURN count(n)')[,1]
# if(dbSize!=0){
#     stop("DODO DB is not empty ==> clean it before loading the content below")
# }
```

## Set DODO instance and version


```r
print(dodoInstance)
```

```
## [1] "UCB-Internal"
```

```r
print(dodoVersion)
```

```
## [1] "2020.10.27"
```

```r
DODO:::set_dodo_version(dodoInstance=dodoInstance, dodoVersion=dodoVersion)
```

## Load Data model


```r
DODO:::load_dodo_model()
```

<!----------------------------------------------------------------->
<!----------------------------------------------------------------->
# Importing Diseases nodes


```r
concept <- "Concept"
```

## Concepts


```r
toImport <- DODO_entryId %>%
  select(
    database = DB,
    shortID = id,
    definition= def,
    label = syn,
    name = dbid,
    level,
    origin
  ) %>%
  arrange(desc(level)) %>%
  filter(database == origin) %>% ## First only DB = origin, to add original DBs, later add additional databases
  distinct(name, .keep_all = T)
toAdd <-  DODO_entryId %>%
  select(
    database = DB,
    shortID = id,
    definition= def,
    label = syn,
    name = dbid,
    level,
    origin
  ) %>%
  arrange(desc(level)) %>%
  filter(database != origin) %>%
  filter(!name %in% toImport$name)
toImport <- bind_rows(toImport, toAdd) %>%
  distinct()
# toImport$origin <- db
concept <- "Concept"
stopifnot(nrow(distinct(toImport, name))==nrow(toImport))
DODO:::load_concept_definitions(toImport=toImport, concept=concept)
```

## Synonyms


```r
toImport <- DODO_idNames %>%
  select("database"=DB, 
         "shortID"=id, 
         "name" = dbid,
         "value"=syn) %>%
  distinct()
DODO:::load_concept_synonyms(toImport=toImport, concept=concept)
```

## Alternative identifiers


```r
toImport <- DODO_altId %>%
  select("database"="DB", 
         "shortID"="id", 
         "altdb"="altDB", 
         "altid"="alt")
DODO:::load_alternative_identifiers(toImport=toImport, concept=concept)
```

## Parents


```r
toImport <- DODO_parentId %>%
  select("database"=DB, 
         "shortID"=id, 
         "parentdb"=pDB, 
         "parentid"=parent,
         origin)
DODO:::load_parent_identifiers(toImport=toImport, concept=concept)
```

## Cross-references


```r
xrefDB <- readxl::read_xlsx(here::here("inst", "documentation",
                                       "CrossreferencesEdges.xlsx"),
                            sheet = "Max_ambiguity_4_across_DBs")
datatable(xrefDB,
          rownames = FALSE)
```

<!--html_preserve--><div id="htmlwidget-11b7b21f6a9e9490dd74" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-11b7b21f6a9e9490dd74">{"x":{"filter":"none","data":[["EFO","MedGen","OPRHA","ORPHA","Cortellis_condition","ORPHA","ORPHA","ORPHA","DOID","EFO","ORPHA","ICD10","Cortellis_indication","ORPHA","ClinVar","Cortellis_indication","MONDO","ORPHA","Cortellis_condition","EFO","DOID","OMIM","MedGen","UMLS","Cortellis_condition","Cortellis_indication","DOID","EFO","ORPHA","ClinVar","MedGen","MONDO","ORPHA","UMLS","ClinVar","DOID","EFO","MedGen","MONDO","ORPHA","UMLS","ClinVar","DOID","MONDO","ORPHA","ClinVar","MedGen","ORPHA"],["ClinVar","ClinVar","ClinVar","Cortellis_condition","Cortellis_indication","Cortellis_indication","DOID","EFO","GARD","GARD","GARD","ICD9","MEDDRA","MedGen","MeSH","MeSH","MeSH","MeSH","MeSH","MeSH","MeSH","MeSH","MeSH","MeSH","MetaBase_disease","MetaBase_disease","MONDO","MONDO","MONDO","NCIt","NCIt","NCIt","NCIt","NCIt","OMIM","OMIM","OMIM","OMIM","OMIM","OMIM","OMIM","SNOMEDCT","SNOMEDCT","SNOMEDCT","SNOMEDCT","UMLS","UMLS","UMLS"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>DB1<\/th>\n      <th>DB2<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

```r
toImport <- DODO_crossId %>%
  select(DB1, id1, DB2, id2)
DODO:::load_cross_references(
  toImport = toImport, 
  xrefDB = xrefDB, 
  concept = concept
)
```

## Diseases - Phenotypes


```r
nodes <- DODO_entryId %>% filter(DB == "HP") %>% pull(dbid)
DODO:::set_labels(concept = "Phenotype", nodes = nodes)

nodes <- DODO_entryId %>% filter(DB != "HP") %>% pull(dbid)
DODO:::set_labels(concept = "Disease", nodes = nodes)

toImport <- DODO_pheno2dis %>%
  select(diseaseDB = disDB, 
         diseaseID = disID, 
         phenoID = phenoID,
         phenoDB = phenoDB)
DODO:::load_has_phenotypes(toImport=toImport)
```

## Connect nodes to databases

Some identifiers are recorded across multiple database, e.g. EFO incorporates identifiers from Monarch, Orphanet, etc. These receive additional *is_in* edge.


```r
# toImport <- bind_rows(DODO_parentId %>% select(DB,
#                                                id, 
#                                                origin),
#                       DODO_parentId %>% select(DB = pDB,
#                                                id = parent,
#                                                origin)) %>%
#   distinct() %>%
#   select(database = DB,
#          shortID = id,
#          origin)
toImport <- DODO_entryId %>%
  select(
    database = DB,
    shortID = id,
    name = dbid,
    level,
    origin
  ) %>%
  arrange(desc(level)) %>%
  distinct(name, origin, .keep_all = T)
DODO:::load_concept_names(toImport = toImport, concept = concept)
```

# Database URL templates



## Write files

Files are writting to the specified location both as rdf and rda files.


```r
############################@
## Save .rda ----
############################@
toSave <- grep("^DODO[_]", ls(), value=T)
save(list = toSave,
     file = here("build","data",
                 paste0(params$name, "-neo4j-input-files.rda")))
```

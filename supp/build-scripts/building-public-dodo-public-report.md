---
title: "Building DODO database"
author: "Liesbeth François"
date: "August 05 2021"
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

workingDirectory <- here("../working")
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
# connect_to_dodo(
#    url = url,
#    importPath = "/data/lfrancois/Development/DODO/build/working/neo4jImport"
#    # importPath = "neo4jImport"
# )
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
```

```
## Warning in scan(file = file, what = what, sep = sep, quote = quote, dec = dec, :
## EOF within quoted string
```

```
## Warning in scan(file = file, what = what, sep = sep, quote = quote, dec = dec, :
## number of items read is not a multiple of the number of columns
```

```
## Warning in scan(file = file, what = what, sep = sep, quote = quote, dec = dec, :
## embedded nul(s) found in input
```

```r
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

preserved49b30027791b17e

**idNames**

Dataframe with columns *DB* (character, database or ontology), *id* (character, identifier without database prefix), *syn* (character, synonym and label), and canonical (boolean, whether *syn* is the canonical concept label).

preserve3527ef2ae650f154

**parentId**

Dataframe with columns *DB* (character, database or ontology), *id* (character, identifier without the database prefex), *pDB* (character, database or ontology of the parent term), *parent* (character, identifiers of the parent), origin (character, database or ontology where this relationship originates from).

preserve5f8bd01da0c445c6

**crossId**

Dataframe with columns *DB1* (character, database or ontology of the first identifier), *id1* (character, first identifier without the database prefex), *DB2* (character, database or ontology of the second identifier), *id2* (character, second identifier also called cross-referenced identifier without database prefix).

preserve90127be8a06ea23c

**altId**

Dataframe with columns *DB* (character, database or ontology), *id* (character, identifier without the database prefex), *alt* (character, database or ontology of the alternative identifier), *altDB* (character, alternative identifier).

preserve952443ce125ecd7f

**pheno2dis**

Dataframe with columns *phenoDB* (character, database or ontology of the phenotype), *phenoID* (character, phenotype identifier without the database prefex), *disDB* (character, database or ontology of the disease identifier), *disID* (character, disease identifier).

preserve4a84e135ece905c0

## Database convention

There are often different abbreviations available for different ontology database. Here, we adopt a naming convention to harmonize the different inputs.

preserved58fbe25dc2a91e6

## Cross-reference edges

There are two types of cross-reference edges encoded into the database, *is_xref* and *is_related*.  The *is_xref* edge is used for equal cross-reference relationships where the concepts relate more directly to each other (similar concept levels). The *is_related* edge is used for all other cross-reference edges. These edges are defined based on the sum of forward and backward ambiguities between databases. Ontologies with a ambiguity equal or lower than 4 are considered as *is_xref* with the exception of ICD10 and ICD9 which are never an *is_xref* edge except between these two databases. In addition, MedGen and UMLS identifiers are duplicated therefore there is an additional *is_xref* edge between these. For more information please consult the vignette. 

preservea2f83e2238dd93ef

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

Semantic types related to "Disease or Syndrome","Acquired Abnormality", "Anatomical Abnormality","Congenital Abnormality", "Neoplastic Process", "Mental or Behavioral Dysfunction" are retained. 


```r
mg <- read.table("/home/lfrancois/Shared/Data-Science/Data-Source-Model-Repository/MedGen/sources/MGSTY.RFF",
                 sep = "|", 
                 header = TRUE, 
                 comment.char = "", 
                 quote = "",
                 fill = TRUE, 
                 colClasses = c("character"))
## Some identifiers have double designation
## Pathologic Function  
## Mental or Behavioral Dysfunction
## Cell or Molecular Dysfunction
toKeep <- mg %>% filter(grepl(paste("Disease or Syndrome","Acquired Abnormality",
                                "Anatomical Abnormality","Congenital Abnormality",
                                "Neoplastic Process","Mental or Behavioral Dysfunction",
                                sep = "|"), STY)) 
## Keep all ClinVar MedGen ids
cv_mg <- cv_crossId %>% 
  filter(DB2 == "UMLS") %>% 
  mutate(DB2 = "MedGen",
         dbid2 = gsub("UMLS", "MedGen", dbid2)) %>% 
  as_tibble()
# toShow <- mg %>% 
#   filter(X.CUI %in% cv_mg$id2)
# sort(table(toShow$STY))
toKeep2 <- mg %>% 
  filter(X.CUI %in% cv_mg$id2)
toRem <- mg %>% 
  filter(!X.CUI %in% c(toKeep$X.CUI, toKeep2$X.CUI))

##
DODO_crossId <- DODO_crossId %>% 
  filter(!dbid2 %in% c(paste("MedGen", toRem$X.CUI, sep = ":"),
                       paste("UMLS", toRem$X.CUI, sep = ":"))) %>%
  filter(!dbid1 %in% c(paste("MedGen", toRem$X.CUI, sep = ":"),
                       paste("UMLS", toRem$X.CUI, sep = ":"))) %>% 
  distinct()
dim(DODO_crossId)
```

```
## [1] 408580      6
```

```r
DODO_entryId <- DODO_entryId %>% 
  filter(!dbid %in% c(paste("MedGen", toRem$X.CUI, sep = ":"),
                       paste("UMLS", toRem$X.CUI, sep = ":")))
DODO_idNames <- DODO_idNames %>% 
  filter(!dbid %in% c(paste("MedGen", toRem$X.CUI, sep = ":"),
                       paste("UMLS", toRem$X.CUI, sep = ":")))
DODO_parentId <- DODO_parentId %>% 
  filter(!pdbid %in% c(paste("MedGen", toRem$X.CUI, sep = ":"),
                       paste("UMLS", toRem$X.CUI, sep = ":"))) %>% 
  filter(!dbid %in% c(paste("MedGen", toRem$X.CUI, sep = ":"),
                       paste("UMLS", toRem$X.CUI, sep = ":")))
DODO_altId <- DODO_altId %>% 
  filter(!dbid %in% c(paste("MedGen", toRem$X.CUI, sep = ":"),
                       paste("UMLS", toRem$X.CUI, sep = ":"))) %>% 
  filter(!adbid %in% c(paste("MedGen", toRem$X.CUI, sep = ":"),
                       paste("UMLS", toRem$X.CUI, sep = ":")))
DODO_pheno2dis <- DODO_pheno2dis %>% 
  filter(!ddbid %in% c(paste("MedGen", toRem$X.CUI, sep = ":"),
                       paste("UMLS", toRem$X.CUI, sep = ":")))
```

## Modify database name

Some resources use either "MedGen" or "UMLS" as database name for NCI Metathesaurus Links. Identifiers from either will therefore be encoded twice into DODO. Similarly, ICD10CM identifiers are also considered as ICD10 identifiers.



```r
##################################################################@
## Modify DB names and entries ----
## Add UMLS entries as MedGen entries (double entry)
DODO_entryId <- DODO_entryId %>% 
  filter(DB == "MedGen") %>% 
  mutate(DB =  str_replace_all(DB, "MedGen","UMLS"),
         origin =  str_replace_all(origin, "MedGen","UMLS"),
         dbid = str_replace_all(dbid, "MedGen","UMLS")) %>%
  bind_rows(DODO_entryId) 
DODO_entryId <- DODO_entryId %>% 
  filter(DB == "UMLS") %>% 
  mutate(DB =  str_replace_all(DB, "UMLS","MedGen"),
         origin =  str_replace_all(origin, "UMLS","MedGen"),
         dbid = str_replace_all(dbid, "UMLS","MedGen")) %>%
  bind_rows(DODO_entryId) %>% 
  distinct()
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
## MedGen to UMLS
DODO_crossId <- filter(DODO_crossId,(DB1 == "MedGen" | DB2 == "MedGen")) %>% 
  mutate(DB1 =  str_replace_all(DB1, "MedGen","UMLS"),
         dbid1 = str_replace_all(dbid1, "MedGen","UMLS"), 
         DB2 =  str_replace_all(DB2, "MedGen","UMLS"),
         dbid2 = str_replace_all(dbid2, "MedGen","UMLS")) %>%
  bind_rows(DODO_crossId) %>%
  bind_rows(tmp)
## UMLS to MedGen
DODO_crossId <- filter(DODO_crossId,(DB1 == "UMLS" | DB2 == "UMLS")) %>% 
  mutate(DB1 =  str_replace_all(DB1, "UMLS","MedGen"),
         dbid1 = str_replace_all(dbid1, "UMLS","MedGen"), 
         DB2 =  str_replace_all(DB2, "UMLS","MedGen"),
         dbid2 = str_replace_all(dbid2, "UMLS","MedGen")) %>%
  bind_rows(DODO_crossId) %>% 
  distinct()
##
DODO_idNames <- filter(DODO_idNames,DB == "MedGen") %>% 
  mutate(DB =  str_replace_all(DB, "MedGen","UMLS"),
         dbid = str_replace_all(dbid, "MedGen","UMLS")) %>%
  bind_rows(DODO_idNames)
DODO_idNames <- filter(DODO_idNames,DB == "UMLS") %>% 
  mutate(DB =  str_replace_all(DB, "UMLS","MedGen"),
         dbid = str_replace_all(dbid, "UMLS","MedGen")) %>%
  bind_rows(DODO_idNames) %>% 
  distinct()
##
DODO_pheno2dis <- filter(DODO_pheno2dis,disDB == "MedGen") %>% 
  mutate(disDB =  str_replace_all(disDB, "MedGen","UMLS"),
         ddbid = str_replace_all(ddbid, "MedGen","UMLS")) %>%
  bind_rows(DODO_pheno2dis)
DODO_pheno2dis <- filter(DODO_pheno2dis,disDB == "UMLS") %>% 
  mutate(disDB =  str_replace_all(disDB, "UMLS","MedGen"),
         ddbid = str_replace_all(ddbid, "UMLS","MedGen")) %>%
  bind_rows(DODO_pheno2dis) %>% 
  distinct()
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

## Check label and definition information


```r
## if identifiers have no definition
table(is.na(DODO_entryId$def))
```

```
## 
##  FALSE   TRUE 
## 420916 407108
```

```r
nodef <- DODO_entryId %>% 
  filter(is.na(def))
table(nodef$DB)
```

```
## 
##                   COHD                     DC               DECIPHER 
##                   1575                    149                      9 
##                  DERMO                     DI                   DOID 
##                      1                      1                  11435 
##                    EFO                     EV                  Fyler 
##                   2843                      2                      1 
##                   GARD                    GTR                   HGNC 
##                   7647                  36285                     47 
##                     HP                  ICD10                ICD10CM 
##                  17798                   4830                   2407 
##                  ICD11                   ICD9                 ICD9CM 
##                      2                   3920                   2234 
##                    IDO                   KEGG                 MedDRA 
##                      2                     38                   1619 
##                 MedGen            MedlinePlus                   MeSH 
##                  24953                      2                  11952 
##                  MFOMD                  MONDO                     MP 
##                      3                  20709                      8 
##                    MTH                   NCIm                   NCIt 
##                      1                      8                  45312 
##                  NDFRT                 NIFSTD                    OAE 
##                      1                     26                      1 
##                    OBI                   OGMS                   OMIM 
##                      2                      2                  60159 
##                 OMIMPS                   OMIT                   OMOP 
##                    498                      2                      5 
##               ONCOTREE                   OROD                  ORPHA 
##                      2                      1                   9287 
##                   PATO                   PMID               Reactome 
##                      1                     67                      1 
##                   SCDO               SNOMEDCT     SNOMEDCT_2010_1_31 
##                      2                 111478                     16 
##    SNOMEDCT_2018_03_01    SNOMEDCT_2020_03_01 SNOMEDCT_US_2018_03_01 
##                      1                      1                      8 
## SNOMEDCT_US_2019_09_01 SNOMEDCT_US_2020_03_01 SNOMEDCT_US_2020_09_01 
##                   4790                      8                      2 
##                   UMLS                UniProt 
##                  24953                      1
```

```r
label <- DODO_idNames %>% 
  filter(canonical == TRUE) %>% 
  filter(dbid %in% nodef$dbid)
table(label$DB)
```

```
## 
##    DOID     EFO      HP   ICD10 ICD10CM  MedGen    MeSH   MONDO    OMIM   ORPHA 
##    9739    2744   15860    5894    2400   22847    4687   20347    9090    9991 
##    UMLS 
##   22847
```

```r
nodef <- nodef %>% 
  mutate(def = label$syn[match(dbid, label$dbid)]) %>% 
  as_tibble()

DODO_entryId <- DODO_entryId %>% 
  filter(!dbid %in% nodef$dbid) %>% 
  bind_rows(nodef) %>% 
  distinct()

## if identifiers have no label but a definition
## 
table(is.na(DODO_idNames$syn))
```

```
## 
##   FALSE 
## 1178493
```

```r
nolabel <- DODO_entryId %>%
  filter(!dbid %in% DODO_idNames$dbid) %>% 
  mutate(canonical = "TRUE") %>% 
  filter(!is.na(def)) %>% 
  select(DB, id, syn = def, canonical, dbid)
head(nolabel)
```

```
##            DB id                                       syn canonical
## ...1 DECIPHER 76              12q14 microdeletion syndrome      TRUE
## ...2 DECIPHER 74            15q13.3 microdeletion syndrome      TRUE
## ...3 DECIPHER 66    15q24 recurrent microdeletion syndrome      TRUE
## ...4 DECIPHER 81                 15q26 overgrowth syndrome      TRUE
## ...5 DECIPHER 68      16p11.2-p12.2 microdeletion syndrome      TRUE
## ...6 DECIPHER 57 17q21.31 recurrent microdeletion syndrome      TRUE
##             dbid
## ...1 DECIPHER:76
## ...2 DECIPHER:74
## ...3 DECIPHER:66
## ...4 DECIPHER:81
## ...5 DECIPHER:68
## ...6 DECIPHER:57
```

```r
table(gsub(":.*", "", unique(nolabel$dbid)))
```

```
## 
## DECIPHER       HP     OMIM    ORPHA 
##       38     1513        6        1
```

```r
head(DODO_idNames)
```

```
##               DB    id                                                syn
## 168323...1 ICD10   A00                                            Cholera
## 168324...2 ICD10 A00.0 Cholera due to Vibrio cholerae 01, biovar cholerae
## 168325...3 ICD10 A00.1    Cholera due to Vibrio cholerae 01, biovar eltor
## 168326...4 ICD10 A00.9                               Cholera, unspecified
## 168327...5 ICD10   A01                     Typhoid and paratyphoid fevers
## 168328...6 ICD10 A01.0                                      Typhoid fever
##            canonical        dbid
## 168323...1      TRUE   ICD10:A00
## 168324...2      TRUE ICD10:A00.0
## 168325...3      TRUE ICD10:A00.1
## 168326...4      TRUE ICD10:A00.9
## 168327...5      TRUE   ICD10:A01
## 168328...6      TRUE ICD10:A01.0
```

```r
dim(DODO_idNames)
```

```
## [1] 1178493       5
```

```r
DODO_idNames <- DODO_idNames %>% 
  bind_rows(nolabel) %>% 
  distinct()
dim(DODO_idNames)
```

```
## [1] 1180051       5
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
   importPath = "/data/lfrancois/Development/working/dodo/neo4jImport"
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
## [1] "UCB-Public"
```

```r
print(dodoVersion)
```

```
## [1] "2021.08.05"
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

preserve26bc1bf5b46e3067

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
     file = here("..","data",
                 paste0(params$name, "-neo4j-input-files.rda")))
```

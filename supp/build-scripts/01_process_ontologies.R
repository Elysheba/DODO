##----------------------------------------------------------------@
##----------------------------------------------------------------@
## Processing files for DODO database"
##----------------------------------------------------------------@
##----------------------------------------------------------------@

# Capture command line arguments
# args <- commandArgs(trailingOnly = TRUE)
# 
# # Check if an argument was passed
# if (length(args) == 0) {
#   stop("No arguments provided. Please provide an argument.")
# }
# 
# # Use the first argument
# params <- args[1]
# cat("Container name provided:", params, "\n")

# .libPaths( c( .libPaths(), "/Local_Disk/Docker_Volumes/Packages") )

params <- "public-dodo"
name <- "dodo"

# In this script, the Dictionary of Disease Ontologies (DODO) database is compiled
# The public version contains only publically available disease ontology (see github)

library(readr)
library(stringr)
library(arrow)
library(data.table)
library(kableExtra)
library(polars)
library(tidyverse)
##

setwd("/Local_Disk/Git_Repos/DODO/")
workingDirectory <- "/Local_Disk/working"

## Specific config
if (params == "internal") {
  dodoInstance <- "UCB-Internal"
} else {
  dodoInstance <- "UCB-Public"
}
## General config
dodoVersion <- format(Sys.Date(), "%Y.%m.%d")
curDate <- Sys.Date()

# Load ontologies

path_name <- "/Local_Disk/Reference_Files/DODO/processed_disease_ontos"
src <- list.dirs(path_name, full.names = F, recursive = F)
for (s in src) {
  ddir <- file.path(path_name, s)
  dbfiles <- list.files(ddir)
  print(s)
  for (i in dbfiles) {
    # print(i)

    if (grepl("CortellisONT", i)) {
      x <- read.table(file.path(ddir, i),
        header = TRUE, sep = "\t",
        quote = '"', comment.char = "",
        colClasses = c("character")
      ) %>%
        as.data.table()
      assign(gsub(".txt", "", i), x)
    } else {
      x <- read_parquet(file.path(ddir, i)) %>%
        as.data.table()

      if (grepl("parentId", i)) {
        x$origin <- s
      }
      if (grepl("entryId", i)) {
        if (grepl("MedGen|ICDO|HP|DO", s)) {
          x$origin <- x$DB
        } else {
          x$origin <- s
        }
      }
    }
    assign(paste(s, gsub(".parquet|.txt", "", i), sep = "_"), x)
  }
}

# Harmonize CortellisONT
if (params == "internal-dodo") {
  # CortellisoNT_siConditions links to CortellisDD and CortellisONT_ciIndications links to CortesllisID
  CortellisONT_siConditions <- copy(CortellisONT_siConditions) %>%
    mutate(
      dbid = paste("Cortellis-condition", id, sep = ":"),
      DB = "Cortellis-condition",
      origin = "Cortellis-condition",
      def = NA,
      name = gsub(paste("\n", "\t", "\r", sep = "|"), " ", tolower(name))
    )
  CortellisONT_ciIndications <- copy(CortellisONT_ciIndications) %>%
    mutate(
      dbid = paste("Cortellis-indication", id, sep = ":"),
      DB = "Cortellis-indication",
      origin = "Cortellis-indication",
      def = NA,
      name = gsub(paste("\n", "\t", "\r", sep = "|"), " ", tolower(name))
    )

  CortellisONT_entryId <- rbind(CortellisONT_siConditions, CortellisONT_ciIndications)

  ## Xref
  crossId1 <- copy(CortellisONT_siConditionXref)[, DB1 := "Cortellis-condition"] %>%
    .[, `:=`(
      dbid1 = paste(DB1, conditionId, sep = ":"),
      dbid2 = paste(source, xref, sep = ":")
    )] %>%
    setnames(., c("source", "xref", "conditionId"), c("DB2", "id2", "id1")) %>%
    .[, -c("dbRef", "direct")] %>%
    .[, `:=`(
      DB2 = gsub("\\_", "-", DB2),
      dbid2 = gsub("\\_", "-", dbid2)
    )]

  crossId2 <- copy(CortellisONT_ciIndicationXref)[, DB1 := "Cortellis-indication"] %>%
    .[, `:=`(
      dbid1 = paste(DB1, indicationId, sep = ":"),
      dbid2 = paste(source, xref, sep = ":")
    )] %>%
    setnames(., c("source", "xref", "indicationId"), c("DB2", "id2", "id1")) %>%
    .[, -c("dbRef", "direct")] %>%
    .[, `:=`(
      DB2 = gsub("\\_", "-", DB2),
      dbid2 = gsub("\\_", "-", dbid2)
    )]
  CortellisONT_crossId <- rbind(crossId1, crossId2)
  rm(crossId1, crossId2)

  ## ================@
  ## ParentId
  parentId1 <- copy(CortellisONT_siConditionParents)[, `:=`(
    DB = "Cortellis-condition",
    pDB = "Cortellis-condition"
  )] %>%
    setnames(., c("depth", "parent"), c("level", "pid")) %>%
    .[, `:=`(
      dbid = paste(DB, id, sep = ":"),
      pdbid = paste(pDB, pid, sep = ":")
    )] %>%
    .[level != 1, ]
  parentId2 <- CortellisONT_ciIndicationParents[, `:=`(
    DB = "Cortellis-indication",
    pDB = "Cortellis-indication"
  )] %>%
    setnames(., c("depth", "parent"), c("level", "pid")) %>%
    .[, `:=`(
      dbid = paste(DB, id, sep = ":"),
      pdbid = paste(pDB, pid, sep = ":")
    )] %>%
    .[level != 1, ]

  CortellisONT_parentId <- rbind(parentId1, parentId2) %>%
    .[, -c("level", "leaf", "treeCode")] %>%
    .[, origin := DB]
  rm(parentId1, parentId2)

  ##
  CortellisONT_idNames <- copy(CortellisONT_entryId[, .(dbid, DB, id, name)]) %>%
    setnames(., c("name"), c("synonym")) %>%
    .[, canonical := TRUE]
}

# Integrate resources
# The input tables from the different ontologies are integrated and checked for consistency.
obj <- c("entryId", "crossId", "idNames", "parentId", "pheno", "altId")
for (i in obj) {
  dbobj <- grep(i, ls(), value = T)
  print(i)
  sobj <- unique(do.call(rbind, lapply(dbobj, function(x) get(x))))
  fn <- paste0("DODO_", i)
  assign(value = sobj, x = fn)
}

##
## Make minor modification to the format
DODO_crossId <- DODO_crossId[!is.na(id2)] %>%
  unique() %>%
  .[, lapply(.SD, function(x) gsub("_", "-", x))]

DODO_pheno <- DODO_pheno[, `:=`(
  dDB = gsub(":.*", "", dbid),
  pDB = gsub(":.*", "", hp),
  did = gsub(".*:", "", dbid),
  pid = gsub(".*:", "", hp)
)] %>%
  setnames(., c("hp"), c("pdbid"))


DODO_altId <- DODO_altId %>%
  setnames(., "altdbib", "adbid") %>%
  .[, lapply(.SD, function(x) gsub("_", "-", x))]

# check availability of def and assign label to def is it is NA
x <- copy(DODO_entryId[, def := case_when(
  is.na(def) ~ name,
  TRUE ~ def
)]) %>%
  .[, same := (DB == origin)] %>%
  .[sort(same), ]
## Adding names as canonical label if missing in idNames
nolabel <- DODO_entryId[!dbid %in% DODO_idNames$dbid]
## if identifiers have no label but a definition
table(is.na(DODO_idNames$syn))
toAdd <- DODO_entryId %>%
  filter(!dbid %in% DODO_idNames$dbid) %>%
  mutate(canonical = TRUE) %>%
  filter(!is.na(def)) %>%
  select(DB, id, synonym = def, canonical, dbid) %>%
  .[, synonym := trimws(gsub("[^[:alnum:]]", " ", synonym))]
DODO_idNames <- DODO_idNames %>%
  bind_rows(toAdd) %>%
  .[, synonym := trimws(gsub(",", " ", synonym))] %>%
  unique() %>%
  .[, lapply(.SD, function(x) gsub("_", "-", x))]

## Add identifiers to DODO_entryId if missing
toAdd <- copy(DODO_crossId[!dbid2 %in% DODO_entryId$dbid, .(dbid2, id2, DB2)]) %>%
  setnames(., c("dbid2", "id2", "DB2"), c("dbid", "id", "DB")) %>%
  .[, `:=`(origin = NA, name = NA, def = NA)] %>%
  unique()
DODO_entryId <- rbind(DODO_entryId, toAdd)

## remove disease ids from dodo_pheno if they are not in dodo_entry as they are not disease ids, but pheno ids themselves
DODO_pheno <- DODO_pheno[dbid %in% DODO_entryId$dbid & pdbid %in% DODO_entryId$dbid] %>%
  .[, lapply(.SD, function(x) gsub("_", "-", x))]

##
toAdd <- copy(DODO_altId[!adbid %in% DODO_entryId$dbid, .(adbid, altDB, altid)]) %>%
  setnames(., c("adbid", "altDB", "altid"), c("dbid", "DB", "id")) %>%
  .[, `:=`(name = NA, origin = NA, def = NA)]
DODO_entryId <- rbind(DODO_entryId, toAdd) %>% unique()

# Some identifiers are multiple times in DODO_entryId
# will check if they have a label/def, if they are the same, if not, we will take the shortest label and combine the definition
DODO_entryId <- copy(DODO_entryId) %>%
  .[, duplicated := .N > 1, by = dbid] %>%
  .[, `:=`(
    name = trimws(gsub("[^[:alnum:]]", " ", name)),
    def = trimws(gsub("[^[:alnum:]]", " ", def))
  )] %>%
  .[, `:=`(
    name = tolower(min(unique(name))),
    def = tolower(paste(unique(na.omit(def)), collapse = " | "))
  ), by = dbid] %>%
  unique() %>%
  .[, lapply(.SD, function(x) gsub("_", "-", x))]

##
## Save files to csv

# Create nodes
instance.nodes <- data.table(
  `name:ID` = "DODO",
  `instance:string` = dodoInstance,
  `version:string` = dodoVersion,
  `:LABEL` = "System"
)

# Create disease nodes
# we are going to add synonyms as an array of terms to the node attribute
table(DODO_entryId$origin)
toAdd <- copy(DODO_idNames) %>%
  .[, syn := tolower(paste0('["', paste(unique(na.omit(synonym)), collapse = '";"'), '"]')), by = dbid] %>%
  .[, N := .N, by = dbid] %>%
  .[, .(dbid, syn, N)] %>%
  unique()
disease.nodes <- DODO_entryId[origin != "HP" | is.na(origin), .(dbid, DB, id, name, def)] %>%
  unique() %>%
  .[, type := case_when(
    DB == "HP" ~ "Disease;Phenotype",
    TRUE ~ "Disease"
  )] %>%
  .[, synonyms := toAdd$syn[match(dbid, toAdd$dbid)]] %>%
  `colnames<-`(c(
    "dbid:ID", "DB:string", "id:string", "name:string",
    "def:string", ":LABEL", "synonyms:string[]"
  ))
phenotype.nodes <- DODO_entryId[
  origin == "HP" & !dbid %in% disease.nodes$`dbid:ID`,
  .(dbid, DB, id, name, def)
] %>%
  unique() %>%
  .[, type := "Phenotype"] %>%
  .[, synonyms := toAdd$syn[match(dbid, toAdd$dbid)]] %>%
  `colnames<-`(c(
    "dbid:ID", "DB:string", "id:string", "label:string",
    "def:string", ":LABEL", "synonyms:string[]"
  ))

databases <- readxl::read_xlsx(here::here("inst", "documentation", "DatabaseURLs.xlsx"),
  col_names = c("name", "idURL")
)
db.nodes <- data.table(db_name = sort(unique(c(
  DODO_entryId[["DB"]],
  DODO_crossId[["DB1"]], DODO_crossId[["DB2"]]
)))) %>%
  .[, DB := gsub("\\_.*", "", db_name)] %>%
  .[, `:=`(
    name = db_name,
    url = databases$idURL[match(DB, databases$name)],
    type = "Database"
  )] %>%
  .[, .(db_name, name, DB, url, type)] %>%
  `colnames<-`(c("db:ID", "name:string", "short_name:string", "url:string", ":LABEL"))

# Create edges
## database of origin
concept_db.edges <- DODO_entryId[!is.na(origin), .(dbid, origin)] %>%
  .[, type := "is_in"] %>%
  `colnames<-`(c(":START_ID", ":END_ID", ":TYPE")) %>%
  unique()
## parent hierarchy
parent.edges <- DODO_parentId[, .(dbid, pdbid, origin)] %>%
  .[, type := "is_a"] %>%
  `colnames<-`(c(":START_ID", ":END_ID", "origin:string", ":TYPE")) %>%
  unique()
table(parent.edges$`:START_ID` %in% c(disease.nodes$`dbid:ID`, phenotype.nodes$`dbid:ID`))
table(parent.edges$`:END_ID` %in% c(disease.nodes$`dbid:ID`, phenotype.nodes$`dbid:ID`))

# xref edges
xrefDB <- readxl::read_xlsx(
  file.path(
    getwd(), "inst", "documentation",
    "CrossreferencesEdges.xlsx"
  ),
  sheet = "Max_ambiguity_4_across_DBs"
) %>%
  as.data.table() %>%
  .[, db_edge := paste(pmin(DB1, DB2), pmax(DB1, DB2), sep = "-")]
xref <- DODO_crossId[DB1 != "HP" & DB2 != "HP", .(dbid1, DB1, dbid2, DB2)] %>%
  .[, type := "is_xref"] %>%
  .[, db_edge := paste(pmin(DB1, DB2), pmax(DB1, DB2), sep = "-")] %>%
  .[, confidence := case_when(db_edge %in% xrefDB$db_edge ~ 1, TRUE ~ 0)]
xref.edges <- rbind(
  xref[, .(dbid1, dbid2, DB2, type, db_edge, confidence)],
  xref[, .(dbid2, dbid1, DB1, type, db_edge, confidence)] %>%
    setnames(., c("dbid1", "dbid2", "DB1"), c("dbid2", "dbid1", "DB2"))
) %>%
  unique() %>%
  .[, ambiguity := .N, by = c("dbid1", "DB2")] %>%
  .[, confidence := case_when(
    ambiguity > 1 ~ 0,
    TRUE ~ confidence
  )] %>%
  .[, .(dbid1, dbid2, type, confidence, ambiguity)] %>%
  .[, type := ifelse(confidence == 1, "is_xref", "is_xref_many")] %>%
  `colnames<-`(c(":START_ID", ":END_ID", ":TYPE", "confidence:int", "ambiguity:int"))
table(xref.edges$`:START_ID` %in% c(disease.nodes$`dbid:ID`, phenotype.nodes$`dbid:ID`))
table(xref.edges$`:END_ID` %in% c(disease.nodes$`dbid:ID`, phenotype.nodes$`dbid:ID`))
# apoc.path.expandConfig can't deal with edge properties.. so let's make two types of edges I guess

## alternative id
altid.edges <- DODO_altId[, .(dbid, adbid)] %>%
  .[, type := "has_alternative_id"] %>%
  `colnames<-`(c(":START_ID", ":END_ID", ":TYPE")) %>%
  unique()
table(altid.edges$`:START_ID` %in% c(disease.nodes$`dbid:ID`, phenotype.nodes$`dbid:ID`))
table(altid.edges$`:END_ID` %in% c(disease.nodes$`dbid:ID`, phenotype.nodes$`dbid:ID`))

## phenotype to disease
pheno.edges <- rbind(
  DODO_pheno[, .(dbid, pdbid)],
  copy(DODO_crossId)[DB1 == "HP", .(dbid2, dbid1)] %>% `colnames<-`(c("dbid", "pdbid")),
  copy(DODO_crossId)[DB2 == "HP", .(dbid1, dbid2)] %>% `colnames<-`(c("dbid", "pdbid"))
) %>%
  .[grepl("HP", pdbid), ] %>%
  .[, type := "has_phenotype"] %>%
  `colnames<-`(c(":START_ID", ":END_ID", ":TYPE")) %>%
  unique()
table(pheno.edges$`:START_ID` %in% c(disease.nodes$`dbid:ID`, phenotype.nodes$`dbid:ID`))
table(pheno.edges$`:END_ID` %in% c(disease.nodes$`dbid:ID`, phenotype.nodes$`dbid:ID`))

toSave <- grep("edges|nodes", ls(), value = T)

walk(toSave, ~ {
  print(.x)
  x <- get(.x)[, lapply(.SD, function(x) gsub(":", "|", x))]
  fwrite(x, file = file.path(
    "/Local_Disk/Reference_Files/DODO/final_disease_ontos",
    params,
    paste0(.x, ".csv")
  ))
})

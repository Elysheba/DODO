#========================================================================================@
#========================================================================================@
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

#========================================================================================@
#========================================================================================@
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


#========================================================================================@
#========================================================================================@
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

#========================================================================================@
#========================================================================================@
#' Harmonize Cortellis information
#' 
#' Harmonizing the Cortellis input information for DODO
#' 
#' @param CortellisONT_siConditions dataframe from CortellisONT siCondition with id (conditionId) and 
#' name (disease label)
#' @param CortellisONT_siConditionParents dataframe from CortellisONT siCondition with id (conditionId), 
#' depth (depth in the hierarchy), leaf, treeCode, parent (parent identifier)
#' @param CortellisONT_siConditionXref dataframe from CortellisONT siCondition with conditionId, source, 
#' xref, dbRef, direct
#' @param CortellisONT_ciIndications dataframe from CortellisONT ciIndication with id (indicationId) and 
#' name (disease label)
#' @param CortellisONT_ciIndicationParents dataframe from CortellisONT ciIndication with id (indicationId), 
#' depth (depth in the hierarchy), leaf, treeCode, parent (parent identifier)
#' @param CortellisONT_ciIndicationXref dataframe from CortellisONT ciIndication with indicationId, source, 
#' xref, dbRef, direct
#' @param tkcon Connection to TKCat instance to check for missing identifiers
#' 
#' @return list with harmonized dataframes
#' 
harmonize_Cortellis <- function(CortellisONT_siConditions,
                                CortellisONT_siConditionParents,
                                CortellisONT_siConditionXref,
                                CortellisONT_ciIndications,
                                CortellisONT_ciIndicationParents,
                                CortellisONT_ciIndicationXref,
                                checkMissing = NULL){
  ## EntryId
  # nc <- nchar(CortellisONT_siConditions$name)
  # head(table(nc), n = 20)
  # CortellisONT_siConditions[which(nc < 4),]
  
  # CortellisONT_siConditions[which(nc < 4),"def"] <- NA
  ## Check characters for \t, \n, \r and put to ASCII
  CortellisONT_siConditions$name <- iconv(x = CortellisONT_siConditions$name,to="ASCII//TRANSLIT")
  CortellisONT_siConditions$name <- gsub(paste("\n","\t","\r", sep = "|")," ",CortellisONT_siConditions$name)
  CortellisONT_siConditions$name <- gsub("\"","'",CortellisONT_siConditions$name)
  CortellisONT_siConditions$name <- gsub("\\\\","",CortellisONT_siConditions$name)
  # table(unlist(sapply(CortellisONT_siConditions$name, strsplit, split = "")))
  
  CortellisONT_entryId <- dplyr::bind_rows(dplyr::mutate(CortellisONT_siConditions %>% 
                                                           dplyr::rename(def = name),
                                                         DB = "Cortellis_condition",
                                                         origin = "Cortellis_condition"),
                                           dplyr::mutate(CortellisONT_ciIndications %>% 
                                                           dplyr::rename(def = name),
                                                         DB = "Cortellis_indication",
                                                         origin = "Cortellis_indication"))  
  # entryId <- mutate(CortellisONT_entryId,
  #                   dbid = str_c(DB, id, sep = ":"))
  
  ## Xref
  crossId1 <- CortellisONT_siConditionXref %>%
    dplyr::filter(direct == TRUE) %>%
    dplyr::mutate(DB1 = "Cortellis_condition") %>%
    dplyr::select(DB1, 
                  id1 = conditionId,
                  DB2 = source,
                  id2 = xref)
  crossId2 <- CortellisONT_ciIndicationXref %>%
    dplyr::filter(direct == TRUE) %>%
    dplyr::mutate(DB1 = "Cortellis_indication") %>%
    dplyr::select(DB1, 
                  id1 = indicationId,
                  DB2 = source,
                  id2 = xref)
  crossId <- dplyr::bind_rows(crossId1,
                              crossId2)
  
  ## crossId
  crossId <- crossId %>%
    dplyr::mutate(dbid1 = stringr::str_c(DB1, id1, sep = ":"),
                  dbid2 = stringr::str_c(DB2, id2, sep = ":")) 
  # crossId$dbid1 <- as.character(crossId$dbid1)
  # crossId$dbid2 <- as.character(crossId$dbid2)
  # table(gsub(":.*","",crossId$dbid2))
  # table(gsub(":.*","",crossId$dbid1))
  crossId <- crossId[!(grepl("#",crossId$dbid1) | grepl("#",crossId$dbid2)),]
  ## Remove crossids with colon and space ": "
  # head(grep(": ",crossId$dbid2,value = T))
  # head(grep(": ",crossId$dbid1,value = T))
  crossId$dbid1 <- gsub(" ", "", crossId$dbid1)
  crossId$dbid2 <- gsub(" ", "", crossId$dbid2)
  ##
  
  ## Remove crossIds without a colon (e.g. definitions, ...)
  # head(grep(":",crossId$dbid1,invert = T,value = T))
  # head(grep(":",crossId$dbid2,invert = T,value = T))
  crossId <- crossId[grepl(":",crossId$dbid2) & grepl(":",crossId$dbid1) ,]
  
  ## an integer is a correct disease ID
  # table(!is.na(as.numeric(crossId$id2)))
  # table(!is.na(as.numeric(crossId$id1)))
  toKeep <- crossId[which(!is.na(as.numeric(crossId$id2)) &
                            !is.na(as.numeric(crossId$id1))),]
  
  toCheck <- crossId[-which(!is.na(as.numeric(crossId$id2)) &
                              !is.na(as.numeric(crossId$id1))),]
  ## When removing prefix, an integer is a correct disease ID
  # table(!is.na(as.numeric(sub("^[^[:digit:]]*", "", toCheck$id2))))
  # table(!is.na(as.numeric(sub("^[^[:digit:]]*", "", toCheck$id1))))
  toKeep <- rbind(toKeep, 
                  toCheck[which(!is.na(as.numeric(sub("^[^[:digit:]]*", "", toCheck$id2))) &
                                  !is.na(as.numeric(sub("^[^[:digit:]]*", "", toCheck$id1)))),])
  
  toCheck <- toCheck[-which(!is.na(as.numeric(sub("^[^[:digit:]]*", "", toCheck$id2))) &
                              !is.na(as.numeric(sub("^[^[:digit:]]*", "", toCheck$id1)))),]
  
  
  ## Remove any DBs that are not disease DBs and DB1 can only be "EFO" or "Orphanet"
  ## check wrong IDs, remove weird ones still
  # table(toCheck$DB2)
  # table(toCheck$DB1)
  toKeep <- dplyr::bind_rows(toKeep,
                             toCheck %>% dplyr::filter(DB2 == "ICD10")) 
  # dplyr::select()
  # dplyr::rename(dbid1 = id1,
  #               dbid2 = id2)
  # table(toKeep$DB2)
  # table(toKeep$DB1)
  crossId <- crossId[grep(paste("http","url","Wikidata",sep = "|"),crossId$id2,invert = T),]
  crossId$id2 <- gsub("MEDDRA","MedDRA",crossId$id2)
  crossId$id2 <- gsub("ORPHANET","ORPHA",crossId$id2)
  crossId$id2 <- gsub("NCI Metathesaurus","NCIt",crossId$id2)
  ## Remove self references
  # crossId[which(crossId$id1 == crossId$id2),]
  
  ## Ids from CortellisDD (Cortellis_condition) are missing
  if(!is.null(checkMissing)){
    missing <- dplyr::filter(checkMissing, 
                             !conditionId %in% crossId$id1)
    toCheck <- CortellisONT_entryId %>%
      dplyr::mutate(dbid = paste(DB, id, sep = ":"))
    
    toAdd <- crossId %>%
      dplyr::filter(dbid2 %in% missing$conditionId) 
    toAdd <- dplyr::inner_join(toAdd,
                               crossId,
                               by = c("dbid1" = "dbid1")) %>%
      dplyr::select(dbid1 = dbid2.x,
                    dbid2 = dbid2.y) %>% 
      dplyr::filter(!stringr::str_detect(dbid2, "Cortellis_condition")) %>%
      dplyr::mutate(DB1 = gsub(":.*", "", dbid1),
                    id1 = gsub(".*:", "", dbid1),
                    DB2 = gsub(":.*", "", dbid2),
                    id2 = gsub(".*:", "", dbid2))
    crossId <- dplyr::bind_rows(crossId,
                                toAdd)
    toAdd <- checkMissing %>% 
      dplyr::mutate(id = stringr::str_remove(conditionId, ".*:"),
                    def = conditionName,
                    DB = "Cortellis_condition",
                    origin = "Cortellis_condition") %>%
      dplyr::select(id, def, DB, origin)
    CortellisONT_entryId <- dplyr::bind_rows(CortellisONT_entryId,
                                             toAdd) %>%
      dplyr::mutate(dbid = paste(DB, id, sep = ":")) %>%
      dplyr::distinct(dbid, .keep_all = TRUE) %>% 
      dplyr::select(-dbid)
  }
  ##
  # crossId <- crossId[-which(crossId$id1 == crossId$id2),]
  crossId$DB2 <- gsub(":.*","",crossId$dbid2)
  crossId$DB1 <- gsub(":.*","",crossId$dbid1)
  crossId$id1 <- gsub(".*:","",crossId$dbid1)
  crossId$id2 <- gsub(".*:","",crossId$dbid2)
  CortellisONT_crossId <- crossId %>%
    dplyr::select(DB1, id1, DB2, id2)
  # crossId <- mutate(crossId,
  #                   dbid1 = str_c(DB1, id1, sep = ":"),
  #                   dbid2 = str_c(DB2, id2, sep = ":"))
  
  
  
  ##================@
  ## ParentId
  parentId1 <- CortellisONT_siConditionParents %>%
    dplyr::mutate(DB = "Cortellis_condition",
                  pDB = "Cortellis_condition",
                  origin = "Cortellis_condition") %>%
    dplyr::select(DB, 
                  id,
                  pDB,
                  parent,
                  origin,
                  level = depth)
  parentId2 <- CortellisONT_ciIndicationParents %>%
    dplyr::mutate(DB = "Cortellis_indication",
                  pDB = "Cortellis_indication",
                  origin = "Cortellis_indication") %>%
    dplyr::select(DB, 
                  id,
                  pDB,
                  parent,
                  origin,
                  level = depth)
  CortellisONT_parentId <- dplyr::bind_rows(parentId1,
                                            parentId2) %>%
    dplyr::filter(!is.na(parent)) %>%
    dplyr::select(-level)
  
  ## add levels to entryID
  tmp <- dplyr::bind_rows(parentId1,
                          parentId2) %>%
    dplyr::mutate(dbid = str_c(DB, id, sep = ":"),
                  level = as.numeric(level)) %>%
    dplyr::select(dbid, level) %>%
    dplyr::arrange(desc(level)) %>%
    dplyr::distinct(dbid, .keep_all = TRUE) %>%
    dplyr::mutate(level = level -1)
  
  CortellisONT_entryId <- CortellisONT_entryId %>%
    dplyr::mutate(dbid = stringr::str_c(DB,id, sep = ":")) %>%
    dplyr::mutate(level = tmp$level[match(dbid, tmp$dbid)]) %>%
    dplyr::select(DB, id, def, level, origin)
  
  ## synonyms and labels
  lbl <- dplyr::bind_rows(dplyr::mutate(CortellisONT_siConditions %>% 
                                          dplyr::rename(syn = name),
                                        DB = "Cortellis_condition",
                                        canonical = TRUE),
                          dplyr:: mutate(CortellisONT_ciIndications %>% 
                                           dplyr::rename(syn = name),
                                         DB = "Cortellis_indication",
                                         canonical = TRUE),
                          dplyr::mutate(cortDD %>% 
                                          dplyr::select(id = conditionId, 
                                                        syn = conditionName),
                                        id = stringr::str_remove(id, ".*:"),
                                        DB = "Cortellis_condition",
                                        canonical = TRUE)) %>%
    dplyr::distinct()
  ##
  syn <- dplyr::bind_rows(dplyr::mutate(CortellisONT_siConditions %>% 
                                          dplyr::rename(syn = name),
                                        DB = "Cortellis_condition",
                                        canonical = FALSE),
                          dplyr::mutate(CortellisONT_ciIndications %>% 
                                          dplyr::rename(syn = name),
                                        DB = "Cortellis_indication",
                                        canonical = FALSE),
                          dplyr::mutate(cortDD %>% 
                                          dplyr::select(id = conditionId, 
                                                        syn = conditionName),
                                        id = stringr::str_remove(id, ".*:"),
                                        DB = "Cortellis_condition",
                                        canonical = FALSE)) %>%
    dplyr::distinct()
  ##
  CortellisONT_idNames <- dplyr::bind_rows(lbl, 
                                           syn) %>%
    dplyr::select(DB, 
                  id , 
                  syn, 
                  canonical)
  
  return(list(CortellisONT_entryId = CortellisONT_entryId,
              CortellisONT_crossId = CortellisONT_crossId,
              CortellisONT_idNames = CortellisONT_idNames,
              CortellisONT_parentId = CortellisONT_parentId
  ))
}

#========================================================================================@
#========================================================================================@
#' Harmonize HPO
#' 
#' Harmoning the parsed files from HPO before merge
#' 
#' @param HPO_diseaseHP Dataframe connecting disease identifiers to HPO identifiers 
#' (columns: db - disease identifier database, id - short disease identifier, hp - short HP identifier)
#' @param HPO_synonyms Dataframe with id (HP short identifier), synonym, and type (of synonym)
#' @param HPO_hp Dataframe with columns id (HP short identifier), name (label information), 
#' description (description of phenotype), level (in hierarchy)
#' @param HPO_altId Dataframe with columns id (HP short identifier) and alt (alternative HP short identifier)
#' @param HPO_diseases Dataframe with columns db (disease identifier database), id (disease short identifier), 
#' label (disease label)
#' @param HPO_parents Dataframe with columns id and parent
#' @param Monarch_hp Dataframe from Monarch Initiative linking MONDO ids to HPO ids (columns DB, id, hp)

harmonize_HPO <- function(HPO_diseaseHP,
                          HPO_synonyms,
                          HPO_hp,
                          HPO_altId,
                          HPO_diseases,
                          HPO_parents,
                          Monarch_hp
){
  ##
  HPO_diseaseHP$id <- as.character(as.integer(HPO_diseaseHP$id))
  
  ## idNames
  HPO_idNames <- HPO_synonyms %>%
    tibble::as_tibble() %>%
    dplyr::select(id, 
                  syn = synonym) %>%
    dplyr::mutate(canonical = FALSE) %>%
    dplyr::bind_rows(HPO_hp %>%
                       dplyr::select(id, syn = name) %>%
                       dplyr::mutate(canonical = TRUE)) %>%
    dplyr::mutate(DB = "HP") %>%
    dplyr::mutate(dbid = str_c(DB, id, sep = ":")) %>%
    dplyr::filter(!is.na(syn)) %>%
    dplyr::arrange(desc(canonical)) %>%
    dplyr::distinct(dbid, syn, .keep_all = TRUE)
  
  ## Entry Id: phenotype + disease ids
  HPO_entryId <- HPO_hp %>%                       ## HP ids
    tibble::as_tibble() %>%
    dplyr::filter(!grepl("obsolete", name)) %>%
    dplyr::select(id, def = description, level) %>%
    dplyr::mutate(DB = "HP",
                  origin = "HP") %>%
    dplyr::bind_rows(tibble::tibble(DB = "HP",                  ## Alternative Ids
                                    id = HPO_altId %>% 
                                      dplyr::select(alt) %>% 
                                      dplyr::distinct() %>%
                                      dplyr::pull(),
                                    def = NA,
                                    level = NA)
    ) %>%
    ## add disease ids from phenotype2disease
    dplyr::bind_rows(HPO_diseaseHP %>% 
                       dplyr::select(DB = db, id) %>% 
                       dplyr::mutate(def = NA, level = NA)) %>% 
    dplyr::bind_rows(HPO_diseases %>% 
                       dplyr::rename(DB = db,
                                     def = label) %>%
                       dplyr::mutate(level = NA)) %>%
    dplyr::mutate(dbid = stringr::str_c(DB, id, sep = ":")) %>%
    dplyr::arrange(def) %>%
    dplyr::distinct(dbid, .keep_all = TRUE)
  
  ## Check characters for \t, \n, \r and put to ASCII
  HPO_entryId$def <- iconv(x = HPO_entryId$def,to="ASCII//TRANSLIT")
  HPO_entryId$def <- gsub(paste("\n","\t","\r", sep = "|")," ",HPO_entryId$def)
  HPO_entryId$def <- gsub(paste("\"","`", sep = "|"),"'", HPO_entryId$def)
  # table(unlist(sapply(HPO_entryId$def, strsplit, split = "")))
  
  ##
  HPO_altId <- HPO_altId %>%
    tibble::as_tibble() %>%
    dplyr::mutate(altDB = "HP",
                  DB = "HP") %>%
    dplyr::distinct()
  
  ##
  HPO_pheno2dis <- HPO_diseaseHP %>%
    tibble::as_tibble() %>%     # add mondo to HP
    dplyr::bind_rows(Monarch_hp %>% 
                       dplyr::select(db = DB, id, hp)) %>%
    dplyr::mutate(phenoDB = "HP") %>%    
    dplyr::select(phenoDB,
                  phenoID = hp, 
                  disDB = db,
                  disID = id) %>%
    dplyr::distinct()
  
  ## parent/child
  HPO_parentId <- HPO_parents %>%
    tibble::as_tibble() %>% 
    dplyr::mutate(DB = "HP", 
                  pDB = "HP", 
                  origin = "HP") %>%
    dplyr::select(DB, 
                  id, 
                  pDB, 
                  parent, 
                  origin) %>%
    dplyr::distinct()
  # table(HPO_parentId$id %in% HPO_entryId$id)
  # table(HPO_parentId$parent %in% HPO_entryId$id)
  
  ## when definition = NA, use label
  tmp <- HPO_idNames %>% 
    dplyr::filter(canonical)
  HPO_entryId <- HPO_entryId %>% 
    dplyr::mutate(def = dplyr::case_when(is.na(def) ~ tmp$syn[match(dbid, tmp$dbid)],
                                         TRUE ~ def))
  ## the alternative HP id doesn't have a label, assign the label from the original id
  tmp <- HPO_altId %>%
    dplyr::inner_join(HPO_idNames,
                      by = "id") %>%
    dplyr::mutate(adbid = paste(altDB, alt, sep = ":"))
  HPO_entryId <- HPO_entryId %>% 
    dplyr::mutate(def = dplyr::case_when(is.na(def) ~ tmp$syn[match(dbid, tmp$adbid)],
                                         TRUE ~ def))
  # table(is.na(HPO_entryId$def))
  
  ## 
  HPO_entryId <- HPO_entryId %>%
    dplyr::select(DB, 
                  id, 
                  def, 
                  level,
                  origin) 
  HPO_idNames <- HPO_idNames %>%
    dplyr::filter(id %in% HPO_entryId$id) %>%
    dplyr::select(DB, id, syn, canonical)
  return(output = list(HPO_idNames = HPO_idNames,
                       HPO_entryId = HPO_entryId,
                       HPO_parentId = HPO_parentId,
                       HPO_pheno2dis = HPO_pheno2dis,
                       HPO_altId = HPO_altId))
}

#========================================================================================@
#========================================================================================@
#' Load Clinvar disease IDs 
#' 
#' Integrate disease identifiers from the ClinVar resource
#' 
#' @param DODO_entryId the object DODO_entryId to filter out existing identifiers
#' @param path_clinvar path to ClinVar parsed files
#' @param traitCref name of the file containing the dataframe with association ClinVar identifiers to external disease identifiers
#' (columns t.id - ClinVar identifier, db - disease database, id - disease identifier, and type)
#' @param traitNames name of the file containing dataframe with columns t.id (ClinVar identifier) and label
#' @param MedGen_MGSTY if provided, file to filter MedGen disease identifiers
#' 
#' @details if MedGen_MGSTY is provided to the MedGen source file MGSTY.RFF, this file will be used to filter 
#' out any MedGen identifiers that are not of type "Disease or Syndrome","Acquired Abnormality", 
#' "Anatomical Abnormality", or "Congenital Abnormality". DODO focusses mainly on disease concepts, so
#' other identifiers types are not considered within the resource.
#' 
#' 
#' @return list with harmonized files
load_ClinVar <- function(DODO_entryId,
                         path_name,
                         traitCref,
                         traitName
                         # MedGen_MGSTY = NULL
                         ){
  clinvar <- read.table(file.path(path_name, traitCref),
                        header = TRUE, 
                        sep = "\t", 
                        quote = '"', 
                        comment.char = "", 
                        colClasses= c("character"))
  clinvar$id <- trimws(clinvar$id)
  clinvar$db <- trimws(clinvar$db)
  clinvar$db <- gsub("Decipher","DECIPHER",clinvar$db)
  clinvar$db <- gsub("MedGen","UMLS",clinvar$db)
  # clinvar$dbid <- paste(clinvar$db, clinvar$id,sep = ":")
  ## Filter on db
  clinvar <- unique(clinvar[clinvar$db %in% c("MeSH","UMLS","OMIM","ORPHA","EFO","DO","NCIt",
                                              "HP","SNOMEDCT","DECIPHER"),])
  
  ## Remove non-disease MedGen
  # if(!is.null(MedGen_MGSTY)){
  #   mg <- read.table(MedGen_MGSTY,
  #                    sep = "|", 
  #                    header = TRUE, 
  #                    comment.char = "", 
  #                    quote = "",
  #                    fill = TRUE, 
  #                    colClasses = c("character"))
  #   mg <- mg[grep(paste("Disease or Syndrome","Acquired Abnormality",
  #                       "Anatomical Abnormality","Congenital Abnormality",
  #                       sep = "|"),mg$STY,invert = T),]
  #   clinvar <- unique(clinvar[!clinvar$id %in% mg$X.CUI,])
  # }
  ## Only keep labels in clinvar
  cv_idNames <- read.table(file.path(path_name, 
                                     traitName),
                           header = TRUE, 
                           sep = "\t", 
                           quote = '"', 
                           comment.char = "", 
                           colClasses= c("character"))
  cv_idNames <- merge(x = cv_idNames, 
                      y = clinvar, 
                      by = "t.id")
  # table(cv_idNames$db[!duplicated(cv_idNames$dbid)])
  
  ## Check labels
  # table(unlist(sapply(cv_idNames$name,strsplit,split = "")))
  # head(grep(":",cv_idNames$dbid,invert = T,value = T))
  ## All numeric ids (without prefix) -- (OK)
  # table(!is.na(as.numeric(cv_idNames$id)))
  # table(!is.na(as.numeric(sub("^[^[:digit:]]*", "", cv_idNames$id))))
  cv_idNames$canonical <- ifelse(cv_idNames$type.x == "Preferred", TRUE, FALSE)
  cv_idNames <- cv_idNames[order(cv_idNames$canonical, decreasing = T),]
  clinvar$def <- cv_idNames$name[match(clinvar$t.id, cv_idNames$t.id)]
  
  ## Create entryId and idNames
  ## Use clinvar internal id and add cross-reference ids (assign label/def)
  cv_entryId <- clinvar %>%
    dplyr::mutate(DB = "ClinVar",
                  id = as.character(t.id),
                  level = NA,
                  origin = "ClinVar",
                  dbid = paste(DB, id, sep = ":")) %>%
    dplyr::select(DB,
                  id, 
                  def,
                  level,
                  origin,
                  dbid) 
  toAdd <- clinvar %>%
    dplyr::select(DB = db,
                  id,
                  def) %>%
    dplyr::mutate(level = NA,
                  origin = DB,
                  dbid = paste(DB, id, sep = ":")) 
  cv_entryId <- dplyr::bind_rows(cv_entryId,
                                 toAdd) %>%
    dplyr::distinct() %>%
    dplyr::filter(!dbid %in% DODO_entryId$dbid)
  ##
  toRet <- cv_idNames %>% 
    dplyr::mutate(DB = "ClinVar",
                  id = as.character(t.id)) %>%
    dplyr::select(DB, 
                  id,
                  syn = name,
                  canonical) %>%
    dplyr::mutate(dbid = paste(DB, id, sep = ":")) %>%
    dplyr::distinct()
  # toAdd <- cv_idNames %>% 
  #   dplyr::mutate(DB = db,
  #                 id) %>%
  #   dplyr::select(DB, 
  #                 id,
  #                 syn = name,
  #                 canonical) %>%
  #   dplyr::mutate(dbid = paste(DB, id, sep = ":")) %>%
  #   dplyr::distinct()
  # cv_idNames <- dplyr::bind_rows(toRet,
  #                                toAdd) %>%
  #   dplyr::filter(!dbid %in% DODO_entryId$dbid)
  cv_idNames <- toRet %>%
    dplyr::filter(!dbid %in% DODO_entryId$dbid)
  ## 
  cv_crossId <- clinvar %>%
    dplyr::mutate(DB1 = "ClinVar",
                  id1 = as.character(t.id)) %>%
    dplyr::select(DB1,
                  id1, 
                  DB2 = db,
                  id2 = id) %>%
    dplyr::distinct() %>%
    dplyr::mutate(dbid1 = paste(DB1, id1, sep = ":"),
                  dbid2 = paste(DB2, id2, sep = ":"))
  return(list(cv_entryId = cv_entryId,
              cv_crossId = cv_crossId,
              cv_idNames = cv_idNames))
}


#========================================================================================@
#========================================================================================@
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
  DODO:::check_df_to_import(toImport, tlc, mandatory)
  
  ## Query ----
  cql <- c(
    'MERGE (db:Database {name:row.name})'
  )
  DODO:::import_in_dodo(cql = neo2R::prepCql(cql), toImport = toImport)
}


#========================================================================================@
#========================================================================================@
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
  concept <- match.arg(concept, c("Concept", "Disease", "Phenotype"))
  if(!"origin" %in% colnames(toImport)){
    toImport$origin <- toImport$database
  }
  if(!"name" %in% colnames(toImport)){
    toImport$name <- paste(toImport$database, toImport$shortID, sep=":")
  }
  if(!"level" %in% colnames(toImport)){
    toImport$level <- NA_integer_
    level = FALSE
  }else{
    level = TRUE
  }
  ## Checks ----
  tlc <- c(
    "database"="character",
    "origin"="character",
    "shortID"="character",
    "name"="character",
    "level" = "integer"
  )
  mandatory <- c(
    "database",
    "shortID"
  )
  DODO:::check_df_to_import(toImport, tlc, mandatory)
  toImport$origin <- ifelse(
    is.na(toImport$origin), toImport$database, toImport$origin
  )
  
  ## Load databases first ----
  dbs <- unique(toImport[, "origin", drop=FALSE])
  colnames(dbs) <- "name"
  DODO:::load_db_names(dbs)
  
  ## Query ----
  cql <- c(
    'MATCH (db:Database {name:row.origin})',
    sprintf('MERGE (c:%s {name:row.name})', concept),
    'SET c.shortID=row.shortID, c.database=row.database',
    sprintf('MERGE (c)-[r:is_in]->(db) %s',
            ifelse(!level, "", 
                   'ON CREATE SET r.level = toInteger(row.level)'))
  )
  DODO:::import_in_dodo(neo2R::prepCql(cql), toImport)
}

#========================================================================================@
#========================================================================================@
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
  concept <- match.arg(concept, c("Concept", "Disease", "Phenotype"))
  if(!"origin" %in% colnames(toImport)){
    toImport$origin <- toImport$database
  }
  if(!"name" %in% colnames(toImport)){
    toImport$name <- paste(toImport$database, toImport$shortID, sep=":")
  }
  ## Checks ----
  tlc <- c(
    "database"="character",
    "origin"="character",
    "name"="character",
    "shortID"="character",
    "label"="character",
    "definition"="character",
    "level"="integer"
  )
  mandatory <- c(
    "database",
    "shortID"
  )
  DODO:::check_df_to_import(toImport, tlc, mandatory)
  toImport$origin <- ifelse(
    is.na(toImport$origin), toImport$database, toImport$origin
  )
  toImport$label_up <- toupper(toImport$label)
  toImport$defintion_up <- toupper(toImport$definition)
  
  ## Load databases first ----
  dbs <- unique(toImport[, "origin", drop=FALSE])
  colnames(dbs) <- "name"
  DODO:::load_db_names(toImport = dbs)
  
  ## Query ----
  cql <- c(
    'MATCH (db:Database {name:row.origin})',
    sprintf('MERGE (c:%s {name:row.name})', concept),
    'SET c.shortID=row.shortID, c.database=row.database,',
    'c.label=row.label, c.label_up=row.label_up, ',
    'c.definition=row.definition, c.definition_up=row.definition_up, ',
    'c.level=toInteger(row.level)',
    'MERGE (c)-[r:is_in]->(db) ON CREATE SET r.level = toInteger(row.level)'
  )
   DODO:::import_in_dodo(neo2R::prepCql(cql), toImport)
}



#========================================================================================@
#========================================================================================@
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
  concept <- match.arg(concept, c("Concept", "Disease", "Phenotype"))
  ## Checks ----
  if(!"name" %in% colnames(toImport)){
    toImport$name <- paste(toImport$database, toImport$shortID, sep=":")
  }
  tlc <- c(
    "database"="character",
    "shortID"="character",
    "value"="character",
    "name"="character"
  )
  mandatory <- c(
    "database",
    "shortID",
    "value"
  )
  DODO:::check_df_to_import(toImport, tlc, mandatory)
  # toImport$name <- paste(toImport$database, toImport$shortID, sep=":")
  toImport$value_up <- toupper(toImport$value)
  toImport <- toImport
  
  ## Concepts ----
  cToImport <- unique(toImport[, c("database", "shortID")])
  DODO:::load_concept_names(cToImport, concept)
  
  ## Query ----
  cql <- c(
    sprintf('MATCH (c:%s {name:row.name})', concept),
    'MERGE (s:Synonym {value:row.value, value_up:row.value_up})',
    'MERGE (c)-[:is_known_as]->(s)'
  )
  DODO:::import_in_dodo(
    neo2R::prepCql(cql),
    toImport[, c("name", "value", "value_up")]
  )
}

#========================================================================================@
#========================================================================================@
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
  concept <- match.arg(concept, c("Concept", "Disease", "Phenotype"))
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
  DODO:::check_df_to_import(toImport, tlc, mandatory)
  tlc <- c(
    "DB1"="character",
    "DB2"="character"
  )
  mandatory <- c(
    "DB1",
    "DB2"
  )
  DODO:::check_df_to_import(xrefDB, tlc, mandatory)
  
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
    DODO:::load_concept_names(cToImport, concept)
    
    ## References ----
    toImport$f <- paste(toImport$DB1, toImport$id1, sep=":")
    toImport$t <- paste(toImport$DB2, toImport$id2, sep=":")
    cql <- c(
      sprintf('MATCH (f:%s {name:row.f})', concept),
      sprintf('MATCH (t:%s {name:row.t})', concept),
      sprintf('MERGE (f)-[:%s]->(t)', type),
      sprintf('MERGE (f)<-[:%s]-(t)', type)
    )
    DODO:::import_in_dodo(neo2R::prepCql(cql), toImport[,c("f", "t")])
    
    ## Update ambiguity ----
    DODO:::call_dodo(neo2R::cypher, query=prepCql(c(
      sprintf('MATCH (c)-[f:%s]->(r)-[b:%s]->(c)', type, type),
      'MATCH (r)-[:is_in]->(d:Database)',
      'WITH c.name AS cname, d.name AS refDB,',
      'count(r) AS rcount,',
      'collect(f) AS allf, collect(b) AS allb',
      'FOREACH(e in allf | set e.FA=rcount)',
      'FOREACH(e in allb | set e.BA=rcount)'
    )))
    DODO:::call_dodo(
      neo2R::cypher,
      query=prepCql(c(
        sprintf(
          'MATCH (f:%s)-[r1:%s {BA:1}]->(t:%s)',
          concept, type, concept
        ),
        sprintf('MERGE (f)-[r2:%s]->(t)', paste0(type, "_nba")),
        'ON CREATE SET r2.BA = r1.BA ON CREATE SET r2.FA = r1.FA'
      ))
    )
    ## when updating new resources, the ambiguity will be calculated on the fly
    ## the old _nba edge might be deprecated and needs to be deleted
    DODO:::call_dodo(
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

#========================================================================================@
#========================================================================================@
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
  concept <- match.arg(concept, c("Concept", "Disease", "Phenotype"))
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
  DODO:::check_df_to_import(toImport, tlc, mandatory)
  toImport$name <- paste(toImport$database, toImport$shortID, sep=":")
  toImport$alt <- paste(toImport$altdb, toImport$altid, sep=":")
  
  ## Concepts ----
  cToImport1 <- toImport[, c("database", "shortID")]
  cToImport2 <- toImport[, c("altdb", "altid")]
  colnames(cToImport1) <- colnames(cToImport2) <- c("database", "shortID")
  cToImport <- unique(rbind(cToImport1, cToImport2))
  DODO:::load_concept_names(cToImport, concept)
  
  ## Query ----
  cql <- c(
    sprintf('MATCH (c:%s {name:row.name})', concept),
    sprintf('MATCH (a:%s {name:row.alt})', concept),
    'MERGE (a)-[:is_alt]->(c)'
  )
  DODO:::import_in_dodo(neo2R::prepCql(cql), 
                        toImport[, c("name", "alt")])
}

#========================================================================================@
#========================================================================================@
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
  DODO:::check_df_to_import(toImport, tlc, mandatory)
  
  ## Query ----
  cql <- c(
    'MERGE (db:Database {name:row.name})',
    'SET db.idURL=row.idURL'
  )
  DODO:::import_in_dodo(neo2R::prepCql(cql), toImport)
}

#========================================================================================@
#========================================================================================@
#' Setting node label in DODO
#'
#' Not exported to avoid unintended modifications of the DB.
#'
set_labels <- function(concept, nodes){
  concept <- match.arg(concept, c("Phenotype", "Disease"), several.ok = FALSE)
  cql <- c('MATCH (p:Concept) WHERE p.name in $from',
           sprintf('SET p:%s', concept))
  DODO:::call_dodo(cypher,
            prepCql(cql),
            parameters = list(from = as.list(nodes)))
}

#========================================================================================@
#========================================================================================@
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
  DODO:::check_df_to_import(toImport, tlc, mandatory)
  toImport$disease <- paste(toImport$diseaseDB, toImport$diseaseID, sep=":")
  toImport$phenotype <- paste(toImport$phenoDB, toImport$phenoID, sep=":")
  
  ## Concepts ----
  diseases <- toImport[, c("diseaseDB", "diseaseID")]
  phenotypes <- toImport[, c("phenoDB", "phenoID")]
  colnames(diseases) <- colnames(phenotypes) <- c("database", "shortID")
  # load_concept_names(diseases, "Disease")
  # load_concept_names(phenotypes, "Phenotype")
  
  ## Query ----
  cql <- c(
    'MATCH (d:Disease {name:row.disease})',
    'MATCH (p:Phenotype {name:row.phenotype})',
    'MERGE (d)-[:has_pheno]->(p)'
  )
  DODO:::import_in_dodo(neo2R::prepCql(cql), toImport[, c("disease", "phenotype")])
}

#========================================================================================@
#========================================================================================@
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
  concept <- match.arg(concept, c("Concept", "Disease", "Phenotype"))
  # stopifnot(is.character(origin), !is.na(origin), length(origin)==1)
  tlc <- c(
    "database"="character",
    "shortID"="character",
    "parentdb"="character",
    "parentid"="character",
    "origin"="character"
  )
  mandatory <- c(
    "database",
    "shortID",
    "parentdb",
    "parentid",
    "origin"
  )
  DODO:::check_df_to_import(toImport, tlc, mandatory)
  toImport$name <- paste(toImport$database, toImport$shortID, sep=":")
  toImport$parent <- paste(toImport$parentdb, toImport$parentid, sep=":")
  
  ## Concepts ----
  cToImport1 <- toImport[, c("database", "shortID")]
  cToImport2 <- toImport[, c("parentdb", "parentid")]
  colnames(cToImport1) <- colnames(cToImport2) <- c("database", "shortID")
  cToImport <- unique(rbind(cToImport1, cToImport2))
  DODO:::load_concept_names(cToImport, concept)
  
  ## Query ----
  cql <- c(
    sprintf('MATCH (c:%s {name:row.name})', concept),
    sprintf('MATCH (p:%s {name:row.parent})', concept),
    sprintf('MERGE (c)-[r:is_a]->(p) ON CREATE SET r.origin = row.origin')
  )
  DODO:::import_in_dodo(neo2R::prepCql(cql), 
                        toImport[, c("name", "parent","origin")])
}



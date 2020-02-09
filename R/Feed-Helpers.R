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

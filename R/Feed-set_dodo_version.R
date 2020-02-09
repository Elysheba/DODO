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

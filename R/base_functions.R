############################################################################### @
#' Find concept by identifier
#'
#' @param ids a character vector of concept to search (e.g. "MONDO:0005027")
#' @param sep String. Separator used, default: ":".
#'
#' @return A character vector of concept identfiers in DODO
#'
#' @examples
#' \dontrun{
#' toRet <- find_id(ids = c("MONDO:0005027", "MONDO:0100033"))
#' }
#'
#' @export
#'
#' @import data.table
#' @import magrittr
find_id <- function(ids, sep = ":") {
  ids <- DODO:::check_divider(ids, sep = sep, format = "in")
  cql <- c(
    "MATCH (n)",
    "WHERE n.dbid IN $from",
    "RETURN n.dbid as dbid"
  )

  toRet <- call_dodo(neo2R::cypher,
    neo2R::prepCql(cql),
    parameters = list(from = as.list(ids)),
    result = "row"
  ) %>%
    data.table::as.data.table() %>%
    .[, dbid := DODO:::check_divider(dbid, format = "out")]

  return(unname(unlist(toRet)))
}

############################################################################### @
#' Find concept by term
#'
#' Querying for concept through search terms (label, synonym)
#'
#' @param term a character vector of terms to search (e.g. "epilep")
#' @param fields the field(s) where to look for matches (label, synonym).
#' @param sep String. Separator used, default: ":".
#'
#' @return A character vector of concept identifiers in DODO
#'
#' @export
#'
#' @import data.table
#' @import magrittr
find_term <- function(term,
                      sep = ":",
                      fields = c("label", "synonym", "definition")) {
  checkmate::assertNames(x = fields, subset.of = c("label", "synonym", "definition"))

  st <- tolower(term)

  query <- NULL
  if ("label" %in% fields) {
    query <- c(
      query,
      sprintf('n.name CONTAINS "%s"', st)
    )
  }
  if ("synonym" %in% fields) {
    query <- c(
      query,
      sprintf(
        'n.synonyms CONTAINS "%s"',
        st
      )
    )
  }
  if ("definition" %in% fields) {
    query <- c(
      query,
      sprintf(
        'n.def CONTAINS "%s"',
        st
      )
    )
  }
  query <- paste(query, collapse = " OR ")

  cql <- c(
    sprintf(
      "MATCH (n) WHERE %s",
      query
    ),
    "RETURN DISTINCT n.dbid as dbid"
  )
  toRet <- call_dodo(neo2R::cypher,
    neo2R::prepCql(cql),
    parameters = NULL,
    result = "row"
  ) %>%
    data.table::as.data.table() %>%
    .[, dbid := DODO:::check_divider(dbid, sep = sep, format = "out")]
  return(unname(unique(unlist(toRet))))
}

# ========================================================================================@
# ========================================================================================@
#' Return entire disease ontology
#'
#' Return all nodes for a particular disease ontology
#'
#' @param database disease ontology to return.
#' @param sep String. Separator used, default: ":".
#'
#' @seealso \code{\link{list_database}}
#'
#' @export
#'
#' @import data.table
#' @import magrittr
get_ontology <- function(database, sep = ":") {
  checkmate::assertNames(database, subset.of = list_database()$database)

  ## Get concepts ----
  cql <- c(
    "MATCH (n)-[:is_in]->(:Database {db:$database})",
    "RETURN DISTINCT n.dbid as dbid"
  )

  ont <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    parameters = list(database = database),
    result = "row"
  )

  cql <- c(
    "MATCH (n)-[r:is_a]-(n1) WHERE n.dbid IN $from AND n1.dbid IN $from",
    "RETURN DISTINCT n.dbid as child, type(r) as type, n1.dbid as parent"
  )

  toRet <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    parameters = list(from = as.list(ont$dbid)),
    result = "row"
  ) %>%
    as.data.table() %>%
    .[, `:=`(
      child = DODO:::check_divider(child, sep = sep, format = "out"),
      parent = DODO:::check_divider(parent, sep = sep, format = "out")
    )]

  ## Return all results ----
  return(toRet)
}

# ========================================================================================@
# ========================================================================================@
#' List of databases in DODO
#'
#' lists all databases present in the database
#' 
#' @return Returns a list of all database in DODO
#'
#' @export
#'
#' @import data.table
#' @import magrittr
list_database <- function() {
  ## Prepare query
  cql <- c(
    "MATCH (n:Database)<-[r:is_in]-(d)",
    "RETURN n.db as database, count(r) as count"
  )
  toRet <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    result = "row"
  ) %>%
    data.table::as.data.table()
  return(toRet)
}

# ========================================================================================@
# ========================================================================================@
#' List of nodes types in DODO
#'
#' Lists the different type of node present in the database
#'
#' @return Returns a list of all database in DODO
#'
#' @export
#'
#' @import data.table
#' @import magrittr
list_node_type <- function() {
  cql <- c(
    "MATCH (n)",
    "RETURN labels(n) as type, count(n) as count"
  )
  toRet <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    result = "row"
  ) %>%
    data.table::as.data.table()
  return(toRet)
}

# ========================================================================================@
# ========================================================================================@
#' Get concept description
#'
#' Describing provided concept identifiers (when available) using disease label
#'
#' @param ids a character vector of  concept identifier to search (e.g. "MONDO:0005027")
#' @param sep String. Separator used, default: ":".
#' @param verbose return query content (default = FALSE)
#'
#' @return data.table with id, label, definition
#'
#' @export
#'
#' @import data.table
#' @import magrittr
describe_concept <- function(ids, sep = ":", verbose = FALSE) {
  if (is.null(ids)) {
    stop("Please provide ID")
  }
  ids <- DODO:::check_divider(ids, format = "in")

  cql <- c(
    "MATCH (n)",
    "WHERE n.dbid IN $from",
    "RETURN n.dbid as dbid, n.name as label, n.def as definition"
  )
  if (verbose) {
    cat(cql, sep = "\n")
  }

  toRet <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    parameters = list(from = as.list(ids)),
    result = "row"
  ) %>%
    data.table::as.data.table() %>%
    .[, dbid := DODO:::check_divider(dbid, sep = sep, format = "out")]

  return(toRet)
}

# ========================================================================================@
# ========================================================================================@
#' Get URL of database
#'
#' Returns a vector of urls of the provided concept identifiers
#'
#' @param ids concept identifiers as vector (e.g. "MONDO:0005027")
#' @param sep String. Separator used, default: ":".
#' @param exact returns url of the original database (e.g. MONDO url for "MONDO:0005027"). Default = TRUE
#'
#' @return Returns a database with dbid, db, id, database, url
#'
#' @examples
#' \dontrun{
#' get_concept_url(ids = "MONDO:0005027")
#' }
#'
#' @export
#'
#' @import data.table
#' @import magrittr
get_concept_url <- function(ids,
                            sep = ":",
                            exact = TRUE) {
  ## Checking
  if (is.null(ids)) {
    stop("Please provide ID")
  }
  ids <- DODO:::check_divider(ids, format = "in")

  cql <- c(
    "MATCH (n)-[r:is_in]->(db:Database)",
    "WHERE n.dbid IN $from",
    "RETURN n.dbid as dbid ,n.DB as db, n.id as id, db.db as origin, db.url as url"
  )
  toRet <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    parameters = list(from = as.list(ids)),
    result = "row"
  ) %>%
    as.data.table() %>%
    .[, `:=`(
      url = sprintf(stringr::str_replace(url, "%3A", "%%3A"), id),
      dbid = DODO:::check_divider(ids, sep = sep, format = "out")
    )]

  if (exact) {
    toRet <- toRet[db == origin, ]
  }
  return(toRet)
}

# ========================================================================================@
# ========================================================================================@
#' Returns current version
#'
#' Returns the version of the current DODO database
#'
#' @return Returns current version of DODO instance
#'
#' @export
#'
#' @import data.table
#' @import magrittr
get_version <- function() {
  cql <- c(
    'MATCH (f {name: "DODO"})',
    "RETURN f.name as Name, f.instance as Instance, f.version as Version"
  )
  toRet <- call_dodo(
    neo2R::cypher,
    neo2R::prepCql(cql),
    result = "row"
  ) %>%
    data.table::as.data.table()
  return(toRet)
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_concept.R
\name{direct_cql}
\alias{direct_cql}
\title{helper function for direct cql query}
\usage{
direct_cql(edge_types = edge_types, to_db = to_db, relationship = relationship)
}
\arguments{
\item{edge_types}{List. Edge types to query. See \link{convert_concept}}

\item{to_db}{String. Database to filter on.}

\item{relationship}{String. Relationship to filter for.}
}
\description{
will return the cql query to get the direct relations
}

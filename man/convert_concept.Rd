% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_concept.R
\name{convert_concept}
\alias{convert_concept}
\title{Convert identifiers}
\usage{
convert_concept(
  ids,
  to_db = NULL,
  relationship = c("xref"),
  direct = F,
  sep = ":",
  verbose = FALSE
)
}
\arguments{
\item{ids}{String. A vector with identifier to convert formatted as DB:id (eg. "MONDO:0005027)}

\item{to_db}{String. A database to convert to (default = NULL, no filtering)}

\item{relationship}{String. Type of relationship to extract ("xref", "parent", "child",
"phenotype", "disease", "alternative"). Default: xref.}

\item{direct}{Boolean. Only getting direct relationships or moving through the disease
network to return all connected relationship.}

\item{sep}{String. Separator used, default: ":".}

\item{verbose}{show query input (default = FALSE)}
}
\value{
a data.table with three columns:
\itemize{
\item from: identifier to convert
\item to: returned conversion
\item relation: type of relation between nodes
}
}
\description{
Convert identifiers between different databases and types (disease or phenotype).
}
\details{
The function identifies cross-references, parent-child hierarchies, disease-phenotype
or alternative relationships.
Direct = T will return only the direct relations, direct = F will move through
the nodes to retrieve connected relationships.
}
\examples{
\dontrun{
convert_concept(
  id = "MONDO:0005027",
  to_db = "EFO",
  relationship = "xref",
  direct = T
)
}
}

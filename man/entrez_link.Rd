\name{entrez_link}
\alias{entrez_link}
\title{Get links to datasets related to an unique ID from an NCBI database}
\usage{
  entrez_link(db, ids, ...)
}
\arguments{
  \item{db}{character Name of the database to search for}

  \item{ids}{integer Id(s) for which data is being
  collected}

  \item{\dots}{character Additional terms to add to the
  request}
}
\value{
  A list containing vectords of unique IDs from different
  databases. the lists are name [db_from]_[db_to]

  file XMLInternalDocument xml file resulting from search,
  parsed with \code{\link{xmlTreeParse}}
}
\description{
  Contstructs a url with the given arguments, and downloads
  xml record returned by that url. See the package-level
  documentation for general advice on using the EUtils
  functions.
}
\examples{
pubmed_search <- entrez_search(db="pubmed", term="Dwarf Elephant", retmax=1)
other_data <- entrez_link(db="all", ids=pubmed_search$ids, dbfrom="pubmed")
}


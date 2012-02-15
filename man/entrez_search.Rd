\name{entrez_search}
\alias{entrez_search}
\title{Search the NCBI databases using EUtils}
\usage{
  entrez_search(db, term, ...)
}
\arguments{
  \item{db}{character Name of the database to search for}

  \item{term}{character The search term}

  \item{\dots}{character Additional terms to add to the
  request}
}
\value{
  ids integer Unique IDS returned by the search

  count integer Total number of hits for the search

  retmax integer Maximum number of hits returned by the
  search

  file XMLInternalDocument xml file resulting from search,
  parsed with \code{\link{XML::xmlTreeParse}}
}
\description{
  Contstructs a url with the given arguments, are downloads
  xml record returned by that url. See the package-level
  documentation for general advice on using the EUtils
  functions.
}
\examples{
pubmed_search <- entrez_search(db="pubmed", term="Dwarf Elephant", retmax=1)
other_data <- entrez_link(db="all", ids=pubmed_search$ids, dbfrom="pubmed")
}


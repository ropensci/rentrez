\name{entrez_summary}
\alias{entrez_summary}
\title{Get summaries of objects in NCBI datasets from a unique ID}
\usage{
  entrez_summary(db, ids, ...)
}
\arguments{
  \item{db}{character Name of the database to search for}

  \item{ids}{integer Id(s) for which data is being
  collected}

  \item{\dots}{character Additional terms to add to the
  request}
}
\value{
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
pubmed_summ <- entrez_summary(db="pubmed", ids=pubmed_search$ids)
}


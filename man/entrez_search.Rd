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

  QueryKey integer identifier for specific query in
  webhistory

  WebEnv character identifier for session key to use with
  history

  file XMLInternalDocument xml file resulting from search,
  parsed with \code{\link{xmlTreeParse}}
}
\description{
  Contstructs a url with the given arguments, are downloads
  xml record returned by that url. See the package-level
  documentation for general advice on using the EUtils
  functions.
}
\examples{
\dontrun{
 ratites <- entrez_search(db="popset", term="ratite", usehistory=TRUE)
  entrez_fetch(db="popset", ids="", file_format="fasta", WebEnv=ratites$WebEnv, query_key=ratites$QueryKey)
}
}


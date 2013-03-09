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
  Contstructs a query with the given arguments, including a
  search term, and a darabase name, then retrieves the XML
  document created by that query. See package-level
  documentation for general advice on using the Entrez
  functions
}
\examples{
\dontrun{
   web_env_search <- entrez_search(db="nuccore",
                                   term="Gastropoda[Organism] AND COI[Gene]",#'                                    usehistory="y")
  cookie <- web_env_search$WebEnv
  qk <- web_env_search$QueryKey
  snail_coi <- entrez_fetch(db = "nuccore", WebEnv = cookie, query_key = qk,
                             file_format = "fasta", retmax = 10)
}
}


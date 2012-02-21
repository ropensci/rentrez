\name{entrez_global_query}
\alias{entrez_global_query}
\title{See how many hits there are a given term across all NCBI Entrez databses}
\usage{
  entrez_global_query(term)
}
\arguments{
  \item{term}{the search term to use}
}
\value{
  a named vector with counts for each a datbase
}
\description{
  Contstructs a url with the given arguments, and downloads
  xml record returned by that url. See the package-level
  documentation for general advice on using the EUtils
  functions.
}
\examples{
mini_elephants <- entrez_global_query(term="Dwarf Elephant")
}


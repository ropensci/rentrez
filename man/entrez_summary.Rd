\name{entrez_summary}
\alias{entrez_summary}
\title{Get summaries of objects in NCBI datasets from a unique ID}
\usage{
  entrez_summary(db, ...)
}
\arguments{
  \item{db}{character Name of the database to search for}

  \item{\dots}{character Additional terms to add to the
  request}
}
\value{
  file XMLInternalDocument xml file resulting from search,
  parsed with \code{\link{xmlTreeParse}}
}
\description{
  Contstructs a query from the given arguments, including a
  database name and list of of unique IDs for that database
  then downloads the XML document created by that query. As
  the XML formats used by various NCBI databases differ
  from each other, no attempt is made to parse the xml
  file. See the package-level documentation for general
  advice on using the EUtils functions.
}
\examples{
\dontrun{
 popset_summ <- entrez_summary(db="popset", id=07082412)
 popset_summ["

}
}


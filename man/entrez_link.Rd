\name{entrez_link}
\alias{entrez_link}
\title{Get links to datasets related to a unique ID from an NCBI database}
\usage{
  entrez_link(db, dbfrom, ...)
}
\arguments{
  \item{db}{character Name of the database to search for
  links (or use "all" to earch all NCBI databases}

  \item{ids}{integer Id(s) for which data is being
  collected}

  \item{dbfrom}{character Name of database from which the
  Id(s) orginate}

  \item{\dots}{character Additional terms to add to the
  request}
}
\value{
  An elink object containing vectors of unique IDs the
  vectors names take the form [db_from]_[db_to]

  file XMLInternalDocument xml file resulting from search,
  parsed with \code{\link{xmlTreeParse}}
}
\description{
  Contstructs a query with the given arguments and
  downloands the XML document created by that query. Unique
  IDs from linked-databases
}
\examples{
\dontrun{
 pubmed_search <- entrez_search(db = "pubmed", term = "10.1016/j.ympev.2010.07.013[doi]")
 linked_data <- entrez_link(dbfrom = "pubmed", id = pubmed_search$ids, db = "all")
nucleotide_IDs <- linked_data$pubmed_nuccore
}
}


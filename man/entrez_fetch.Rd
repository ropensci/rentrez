\name{entrez_fetch}
\alias{entrez_fetch}
\title{Download data from NCBI databases}
\usage{
  entrez_fetch(db, ids, file_format, ...)
}
\arguments{
  \item{db}{character Name of the database to use}

  \item{ids}{integer Unique ID(s) to get data from}

  \item{file_format}{character Format in which to get data
  (eg, fasta, xml...)}

  \item{\dots}{character Additional terms to add to the
  request}
}
\value{
  chracter string containing the file created
}
\description{
  Download data from NCBI databases
}
\examples{
snail_search <- entrez_search("nuccore", "Gastropoda[Organism]")
records <- entrez_fetch("nuccore", snail_search$ids[1:2], "fasta")
}


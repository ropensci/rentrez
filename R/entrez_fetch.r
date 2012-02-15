#' Download data from NCBI databases
#'
#'
#'
#'@export
#'@param db character Name of the database to use
#'@param ids integer Unique ID(s) to get data from
#'@param file_format character Format in which to get data (eg, fasta, xml...)
#'@param \dots character Additional terms to add to the request
#'@return chracter string containing the file created
#' @examples
#' 
#' snail_search <- entrez_search("nuccore", "Gastropoda[Organism]")
#' records <- entrez_fetch("nuccore", snail_search$ids[1:2], "fasta")



entrez_fetch <- function(db, ids, file_format, ...){
    args <- c(db=db, id=paste(ids, collapse=","), rettype=file_format, 
              email=entrez_email, tool=entrez_tool, ...)
    url_args <- paste(paste(names(args), args, sep="="), collapse="&")
    base_url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?retmode=text"
    url_string <- paste(base_url, url_args, sep="&")
    records <- getURL(url_string)
    return(records)
}

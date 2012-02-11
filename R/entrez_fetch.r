#' Search the NCBI's entrez database
#'
#'
#'
#'
#'
#'
#'
#'name

entrez_fetch <- function(db, ids, file_format, ...){
    args <- c(db=db, id=paste(ids, collapse=","), rettype=file_format, 
              email=entrez_email, tool=entrez_tool, ...)
    url_args <- paste(paste(names(args), args, sep="="), collapse="&")
    base_url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?retmode=text"
    url_string <- paste(base_url, url_args, sep="&")
    records <- getURL(url_string)
    return(records)
}

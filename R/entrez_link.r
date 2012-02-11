entrez_link <- function(dbfrom, ids, ...){
    args <- c(dbfrom=dbfrom, id=paste(ids, collapse=","), 
              email=entrez_email, tool=entrez_tool, ...)
    url_args <- paste(paste(names(args), args, sep="="), collapse="&")
    base_url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?"
    url_string <- paste(base_url, url_args, sep="&")
    record <- xmlParse(getURL(url_string))
    return(record)
}

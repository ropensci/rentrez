#' Search the NCBI's entrez database
#'
#'
#'
#'
#'
#'
#'
#'name

entrez_search <- function(db, term, ... ){
    args <- c(db=db, term=term, email=entrez_email, tool=entrez_tool, ...)
    url_args <- paste(paste(names(args), args, sep="="), collapse="&")
    base_url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?"
    search <- paste(base_url, url_args, sep="&")
    xml_result <- xmlParse(getURL(search))
    ids <- xpathSapply(xml_result, "//IdList/Id", xmlValue))
    count <- xpathSapply(xml_result, "/eSearchResult/Count", fun=xmlValue))
    retmax <- xpathSapply(xml_result, "/eSearchResult/RetMax", fun=xmlValue))
    return(list(file=xml_result, ids=as.integer(ids), 
                count=as.integer(count), 
                retmax=as.integer(retmax)
            ))
}

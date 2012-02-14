entrez_link <- function(db, ids, ...){
    args <- c(db=db, id=paste(ids, collapse=","), 
              email=entrez_email, tool=entrez_tool, ...)
    url_args <- paste(paste(names(args), args, sep="="), collapse="&")
    base_url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?"
    url_string <- paste(base_url, url_args, sep="&")
    record <- xmlTreeParse(getURL(url_string), useInternalNodes=TRUE)
    db_names <- xpathSApply(record, "//LinkName", xmlValue)
    get_Ids <- function(dbname){
        path <-  paste("//LinkSetDb/LinkName[text()='", dbname, "']/../Link/Id", sep="")
        return(xpathSApply(record, path, xmlValue))
    }
    # Get ID from each database
    # Not simplified so if a single database get a named list (for consistancy)
    result <- sapply(db_names, get_Ids, simplify=FALSE) 
    result$file <- record
    return(result)
}

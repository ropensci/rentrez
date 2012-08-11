#' Search the NCBI databases using EUtils
#'
#' Contstructs a url  with the given arguments, are downloads xml record
#' returned by that url. See the package-level documentation for general advice
#' on using the EUtils functions. 
#'
#'
#'@export
#'@param db character Name of the database to search for
#'@param term character The search term
#'@param \dots character Additional terms to add to the request 
#
#'@return ids integer Unique IDS returned by the search
#'@return count integer Total number of hits for the search
#'@return retmax integer Maximum number of hits returned by the search
#'@return QueryKey integer identifier for specific query in webhistory
#'@return WebEnv character identifier for session key to use with history
#'@return file XMLInternalDocument xml file resulting from search, parsed with
#'\code{\link{xmlTreeParse}}
#' @examples
#' \dontrun{
#'  ratites <- entrez_search(db="popset", term="ratite", usehistory=TRUE)
#'   entrez_fetch(db="popset", ids="", file_format="fasta", WebEnv=ratites$WebEnv, query_key=ratites$QueryKey)
#'}
#'
#' 


entrez_search <- function(db, term, ... ){
    args <- c(db=db, term=gsub(" ", "+", term), email=entrez_email, 
              tool=entrez_tool, ...)
    url_args <- paste(paste(names(args), args, sep="="), collapse="&")
    base_url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?"
    search <- paste(base_url, url_args, sep="&")
    xml_result <- xmlParse(getURL(search))
    ids <- xpathSApply(xml_result, "//IdList/Id", xmlValue)
    count <- xpathSApply(xml_result, "/eSearchResult/Count", xmlValue)
    retmax <- xpathSApply(xml_result, "/eSearchResult/RetMax", xmlValue)
    QueryKey <- xpathSApply(xml_result, "/eSearchResult/QueryKey", xmlValue)
    WebEnv <- xpathSApply(xml_result, "/eSearchResult/WebEnv", xmlValue)

    #NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(list(file=xml_result, ids=as.integer(ids), 
                count=as.integer(count), 
                retmax=as.integer(retmax),
                QueryKey= as.integer(QueryKey),
                WebEnv = WebEnv
            ))
}

#' Search the NCBI databases using EUtils
#'
#' Contstructs a query with the given arguments, including a search term, and
#' a darabase name, then retrieves the XML document created by that query. 
#' See package-level documentation for general advice on using the Entrez functions 
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
#'@import RCurl XML
#'@return file XMLInternalDocument xml file resulting from search, parsed with
#'\code{\link{xmlTreeParse}}
#' @examples
#' \dontrun{
#'    query <- "Gastropoda[Organism] AND COI[Gene]"
#'    web_env_search <- entrez_search(db="nuccore", query, usehistory="y")
#'    cookie <- web_env_search$WebEnv
#'    qk <- web_env_search$QueryKey 
#'    snail_coi <- entrez_fetch(db = "nuccore", WebEnv = cookie, query_key = qk,
#'                              file_format = "fasta", retmax = 10)
#'}

entrez_search <- function(db, term, ... ){
    response <- make_entrez_query("esearch", db=db, 
                                    term=gsub(" ", "+", term), ...)
    xml_result <- xmlParse(response)
    ids <- xpathSApply(xml_result, "//IdList/Id", xmlValue)
    count <- xpathSApply(xml_result, "/eSearchResult/Count", xmlValue)
    retmax <- xpathSApply(xml_result, "/eSearchResult/RetMax", xmlValue)
    QueryKey <- xpathSApply(xml_result, "/eSearchResult/QueryKey", xmlValue)
    WebEnv <- xpathSApply(xml_result, "/eSearchResult/WebEnv", xmlValue)

    #NCBI limits requests to three per second
    Sys.sleep(0.33)
    res <- (list(file=xml_result, ids=ids, 
                 count=as.integer(count), 
                 retmax=as.integer(retmax),
                 QueryKey= as.integer(QueryKey),
                 WebEnv = WebEnv
            ))
    class(res) <- c("esearch", class(res))
    return(res)
}

#' @export

print.esearch <- function(x, ...){
    cat(paste("Entrez search result with", x$count, "IDs (max =", x$retmax, ")\n"))
}


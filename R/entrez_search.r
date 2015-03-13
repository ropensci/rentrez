#' Search the NCBI databases using EUtils
#'
#' Constructs a query with the given arguments, including a search term, and
#' a database name, then retrieves the XML document created by that query. 
#' 
#'@export
#'@param db character Name of the database to search for
#'@param term character The search term
#'@param \dots character Additional terms to add to the request 
#'@param retmode character One of json (default) or xml. This will make no
#' difference in most cases.
#'@param config vector configuration options passed to httr::GET  
#'@seealso \code{\link[httr]{config}} for available configs 
#'@seealso \code{\link{entrez_db_searchable}} to get a set of search fields that
#' can be used in \code{term} for any base
#'@return ids integer Unique IDS returned by the search
#'@return count integer Total number of hits for the search
#'@return retmax integer Maximum number of hits returned by the search
#'@return QueryKey integer identifier for specific query in webhistory
#'@return WebEnv character identifier for session key to use with history
#'@return QueryTranslation character, search term as the NCBI interpreted it
#'@import  XML
#'@return file either and XMLInternalDocument xml file resulting from search, parsed with
#'\code{\link[XML]{xmlTreeParse}} or, if \code{retmode} was set to json a list
#' resulting from the returned JSON file being parsed with
#' \code{\link[jsonlite]{fromJSON}}.
#' @examples
#' \dontrun{
#'    query <- "Gastropoda[Organism] AND COI[Gene]"
#'    web_env_search <- entrez_search(db="nuccore", query, usehistory="y")
#'    cookie <- web_env_search$WebEnv
#'    qk <- web_env_search$QueryKey 
#'    snail_coi <- entrez_fetch(db = "nuccore", WebEnv = cookie, query_key = qk,
#'                              file_format = "fasta", retmax = 10)
#'}
#'\donttest{
#' 
#' fly_id <- entrez_search(db="taxonomy", term="Drosophila")
#' #Oh, right. There is a genus and a subgenus name Drosophila...
#' #how can we limit this search
#' (tax_fields <- entrez_db_searchable("taxonomy"))
#' #"RANK" loots promising
#' tax_fields$RANK
#' entrez_search(db="taxonomy", term="Drosophila & Genus[RANK]")
#'}

entrez_search <- function(db, term, config=NULL, retmode="xml", ... ){
    response <- make_entrez_query("esearch", 
                                  db=db, 
                                  term=term, 
                                  config=config,
                                  retmode=retmode, 
                                  ...)
    parsed <- parse_response(response, retmode)
    parse_esearch(parsed)
}


parse_esearch <- function(x) UseMethod("parse_esearch")
   
parse_esearch.XMLInternalDocument <- function(x){
    res <- list( ids      = XML::xpathSApply(x, "//IdList/Id", XML::xmlValue),
                 count    = XML::xpathSApply(x, "/eSearchResult/Count", XML::xmlValue),
                 retmax   = XML::xpathSApply(x, "/eSearchResult/RetMax", XML::xmlValue),
                 QueryKey = XML::xpathSApply(x, "/eSearchResult/QueryKey", XML::xmlValue),
                 WebEnv   = XML::xpathSApply(x, "/eSearchResult/WebEnv", XML::xmlValue),
                 QueryTranslation   = XML::xpathSApply(x, "/eSearchResult/QueryTranslation",XML::xmlValue),
                 file     = x)
    res <- Filter(function(x) length(x) > 0, res)
    class(res) <- c("esearch", "list")
    return(res)
}

parse_esearch.list <- function(x){
    res <- x$esearchresult[ c("idlist", "count", "retmax", "querykey", "webenv", "querytranslation") ]
    names(res)[c(1,4:6)] <- c("ids", "QueryKey", "WebEnv", "QueryTranslation")
    res <- Filter(function(x) !is.null(x), res)
    res$file <- x
    class(res) <- c("esearch", "list")
    return(res)
}

#'@export
print.esearch <- function(x, ...){
    display_term <- if(nchar(x$QueryTranslation) > 50){
        paste(substr(x$QueryTranslation, 1, 50), "...")
    } else x$QueryTranslation
    cookie_word <- if("WebEnv" %in% x$ids) "a" else "no"
    msg<- paste("Entrez search result with", x$count, "hits (object contains",
                length(x$ids), "IDs and", cookie_word, 
                "cookie)\n Search term (as translated): "  , display_term, "\n")
    cat(msg)
}
 

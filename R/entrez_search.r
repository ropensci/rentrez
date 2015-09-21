#' Search the NCBI databases using EUtils
#'
#' The NCBI uses a search term syntax where search terms can be associated with 
#' a specific search field with square brackets. So, for instance ``Homo[ORGN]''
#' denotes a search for Homo in the ``Organism'' field. The names and
#' definitions of these fields can be identified using
#' \code{\link{entrez_db_searchable}}.
#'
#' Searches can make use of several fields by combining them via the boolean
#' operators AND, OR and NOT. So, using the search term``((Homo[ORGN] AND APP[GENE]) NOT
#' Review[PTYP])'' in PubMed would identify articles matching the gene APP in
#' humans, and exclude review articles. More examples of the use of these search
#' terms, and the more specific MeSH terms for precise searching, 
#' is given in the package vignette.
#' 
#'@export
#'@param db character, name of the database to search for.
#'@param term character, the search term.
#'@param use_history logical. If TRUE return a web_history object for use in 
#' later calls to the NCBI
#'@param retmode character, one of json (default) or xml. This will make no
#' difference in most cases.
#'@param \dots characte, additional terms to add to the request, see NCBI
#'documentation linked to in references for a complete list
#'@param config vector configuration options passed to httr::GET  
#'@seealso \code{\link[httr]{config}} for available httr configurations 
#'@seealso \code{\link{entrez_db_searchable}} to get a set of search fields that
#' can be used in \code{term} for any database
#'@return ids integer Unique IDS returned by the search
#'@return count integer Total number of hits for the search
#'@return retmax integer Maximum number of hits returned by the search
#'@return web_history A web_history object for use in subsequent calls to NCBI
#'@return QueryTranslation character, search term as the NCBI interpreted it
#'@return file either and XMLInternalDocument xml file resulting from search, parsed with
#'\code{\link[XML]{xmlTreeParse}} or, if \code{retmode} was set to json a list
#' resulting from the returned JSON file being parsed with
#' \code{\link[jsonlite]{fromJSON}}.
#'@references \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#_chapter4_ESearch_} 
#'@examples
#' \dontrun{
#'    query <- "Gastropoda[Organism] AND COI[Gene]"
#'    web_env_search <- entrez_search(db="nuccore", query, use_history=TRUE)
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

entrez_search <- function(db, term, config=NULL, retmode="xml", use_history=FALSE, ... ){
    usehistory <- if(use_history) "y" else "n"
    response <- make_entrez_query("esearch", 
                                  db=db, 
                                  term=term, 
                                  config=config,
                                  retmode=retmode, 
                                  usehistory=usehistory,
                                  ...)
    parsed <- parse_response(response, retmode)
    parse_esearch(parsed, history=use_history)
}


parse_esearch <- function(x, history) UseMethod("parse_esearch")
   
parse_esearch.XMLInternalDocument <- function(x, history){
    res <- list( ids      = xpathSApply(x, "//IdList/Id", xmlValue),
                 count    = as.integer(xmlValue(x[["/eSearchResult/Count"]])),
                 retmax   = as.integer(xmlValue(x[["/eSearchResult/RetMax"]])),
                 QueryTranslation   = xmlValue(x[["/eSearchResult/QueryTranslation"]]),
                 file     = x)
    if(history){
        res$web_history = web_history(
          QueryKey = xmlValue(x[["/eSearchResult/QueryKey"]]),
          WebEnv   = xmlValue(x[["/eSearchResult/WebEnv"]])
        )
    }
    class(res) <- c("esearch", "list")
    return(res)
}

parse_esearch.list <- function(x, history){
    #for consitancy between xml/json records we are going to change the
    #file names from lower -> CamelCase
    res <- x$esearchresult[ c("idlist", "count", "retmax", "querytranslation") ]
    names(res)[c(1,4)] <- c("ids", "QueryTranslation")
    if(history){
        res$web_history = web_history(QueryKey = x$esearch_result[["querykey"]], 
                                      WebEnv   = x$esearch_result[["webenv"]])
    }
    res$count <- as.integer(res$count)
    res$retmax <- as.integer(res$retmax)
    res$file <- x
    class(res) <- c("esearch", "list")
    return(res)
}

#'@export
print.esearch <- function(x, ...){
    display_term <- if(nchar(x$QueryTranslation) > 50){
        paste(substr(x$QueryTranslation, 1, 50), "...")
    } else x$QueryTranslation
    cookie_word <- if("web_history" %in% names(x)) "a" else "no"
    msg<- paste("Entrez search result with", x$count, "hits (object contains",
                length(x$ids), "IDs and", cookie_word, 
                "web_history object)\n Search term (as translated): "  , display_term, "\n")
    cat(msg)
}


 c("//IdList/Id", "/eSearchResult/Count", "/eSearchResult/RetMax", "/eSearchResult/QueryTranslation")

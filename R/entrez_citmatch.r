#' Fetch pubmed ids matching specially formatted citation strings
#'
#'@param bdata character, containing citation data. 
#' Each citation must be represented in a pipe-delimited format
#'  journal_title|year|volume|first_page|author_name|your_key|
#' The final field "your_key" is arbitrary, and can used as you see
#' fit. Fields can be left empty, but be sure to keep 6 pipes.
#'@param db character, the database to search. Defaults to pubmed,
#' the only database currently available
#'@param retmode character, file format to retrieve. Defaults to xml, as 
#' per the API documentation, though note the API only returns plain text
#'@param config vector configuration options passed to httr::GET  
#'@return A character vector containing PMIDs
#'@seealso \code{\link[httr]{config}} for available configs
#'@export
#'@examples
#'\donttest{
#' ex_cites <- c("proc natl acad sci u s a|1991|88|3248|mann bj|test1|",
#'               "science|1987|235|182|palmenberg ac|test2|")
#' entrez_citmatch(ex_cites)
#'}
entrez_citmatch <- function(bdata, db="pubmed", retmode="xml", config=NULL){
    if(length(bdata) > 1){
        bdata <- paste0(bdata, collapse="\r")
    }
    ifelse(.last(bdata)=="|", bdata, paste0(bdata, "|"))
    request <- make_entrez_query("ecitmatch", 
                                 bdata=bdata, 
                                 db=db, 
                                 retmode=retmode,
                                 interface=".cgi?",
                                 config=config)
    results <- strsplit(strsplit(request, "\n")[[1]], "\\|")
    sapply(results, extract_pmid)
}

extract_pmid <- function(line){
    tryCatch("[["(line,7), 
            error=function(e){
                warning(paste("No pmid found for line", line))
                NA
            }
   )
        
}

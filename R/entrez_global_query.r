#' See how many hits there are for a given term across all NCBI Entrez databses
#'
#' 
#'
#'@export
#'@param term the search term to use
#'@param ... additional arguments to add to the query
#'@return a named vector with counts for each a datbase
#'
#'@import RCurl XML
#' @examples
#' 
#' NCBI_data_on_best_butterflies_ever <- entrez_global_query(term="Heliconius")

entrez_global_query <- function(term, ...){
    response <- make_entrez_query("egquery", 
                                    term=gsub(" ", "+", term), 
                                    ...)
    record <- xmlTreeParse(response, useInternalNodes=TRUE)
    db_names <- xpathSApply(record, "//ResultItem/DbName", xmlValue)
    get_Ids <- function(dbname){
        path <-  paste("//ResultItem/DbName[text()='", dbname, "']/../Count", sep="")
        res <- as.numeric(xpathSApply(record, path, xmlValue))
    }
    #NCBI limits requests to three per second
    Sys.sleep(0.33)
    res <- structure(sapply(db_names, get_Ids), names=db_names)
    return(res)
}

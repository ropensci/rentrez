#' Search the NCBI's entrez database
#'
#'
#'
#'
#'
#'
#'
#'name

search_entrez <- function(dbase, term, retmax=6,...){
    base_url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=%s&term=%s&retmax=%i"
    search <- sprintf(base_url, dbase, term, retmax)
    raw_result <- getURL(search)
    ids <- unlist(getNodeSet(xmlParse(raw_xml), "//Id", fun=xmlValue))
    return(as.integer(ids))
}

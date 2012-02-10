#' Search the NCBI's entrez database
#'
#'
#'
#'
#'
#'
#'
#'name

fetch_entrez <- function(dbase, ids, format...){
    base_url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=%s&id=%s&rettype=%s&retmode=text"
    url_string <- sprintf(base_url, dbase, paste(ids, collapse=","), format)
    records <- getURL(url_string)
    return(records)

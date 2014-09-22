#' Download data from NCBI databases
#'
#'
#'
#'@export
#'@param db character Name of the database to use
#'@param rettype character Format in which to get data (eg, fasta, xml...)
#'@param retmode character Mode in which to recieve data, defaults to 'text'
#'@param \dots character Additional terms to add to the request
#'@return chracter string containing the file created
#'@import RCurl
#' @examples
#' 
#' katipo <- "Latrodectus katipo[Organism]"
#' katipo_search <- entrez_search(db="nuccore", term=katipo)
#' kaitpo_seqs <- entrez_fetch(db="nuccore", id=katipo_search$ids, rettype="fasta")
#'

entrez_fetch <- function(db, rettype, retmode="text", ...){
    records <- make_entrez_query("efetch", db=db, rettype=rettype, ..., require_one_of = c("id", "WebEnv"))
    #NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(records)
}

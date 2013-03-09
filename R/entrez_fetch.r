#' Download data from NCBI databases
#'
#'
#'
#'@export
#'@param db character Name of the database to use
#'@param ids integer Unique ID(s) to get data from (required if WebEnv not used)
#'@param rettype character Format in which to get data (eg, fasta, xml...)
#'@param \dots character Additional terms to add to the request
#'@return chracter string containing the file created
#' @examples
#'\dontrun{ 
#'    katipo <- "Latrodectus katipo[Organism]"
#'    katipo_search <- entrez_search(db="nuccore", term=katipo)
#'    kaitpo_seqs <- entrez_fetch(db="nuccore", ids=katipo_search$ids, format="fasta")
#'}

entrez_fetch <- function(db, rettype, retmode="text", ...){
    url_string <- make_entrez_query("efetch", 
          require_one_of = c("id", "WebEnv"), db=db, ..., rettype=rettype)
    records <- getURL(url_string)
    #NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(records)
}

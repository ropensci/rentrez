#' Download data from NCBI databases
#'
#'
#'
#'@export
#'@param db character Name of the database to use
#'@param ids integer Unique ID(s) to get data from (required if WebEnv not used)
#'@param file_format character Format in which to get data (eg, fasta, xml...)
#'@param \dots character Additional terms to add to the request
#'@return chracter string containing the file created
#' @examples
#'\dontrun{ 
#'    katipo <- "Latrodectus katipo[Organism]"
#'    katipo_search <- entrez_search(db="nuccore", term=katipo)
#'    kaitpo_seqs <- entrez_fetch(db="nuccore", ids=katipo_search$ids, format="fasta")
#'}

entrez_fetch <- function(db, ids="", file_format, ...){
      
    args <- c(db=db, rettype=file_format, 
              email=entrez_email, tool=entrez_tool, ...)
	    args <- c(id=paste(ids, collapse=","),args)
    
    url_args <- paste(paste(names(args), args, sep="="), collapse="&")
    base_url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?retmode=text"
    url_string <- paste(base_url, url_args, sep="&")
    records <- getURL(url_string)
    #NCBI limits requests to three per second
    Sys.sleep(0.33)
    return(records)
}

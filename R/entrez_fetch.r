#' Download data from NCBI databases
#'
#'
#'
#'@export
#'@param db character Name of the database to use
#'@param rettype character Format in which to get data (eg, fasta, xml...)
#'@param retmode character Mode in which to receive data, defaults to 'text'
#'@param \dots character Additional terms to add to the request
#'@param config vector configuration options passed to httr::GET
#'@param parsed boolean should entrez_fetch attempt to parse the resulting 
#' file. Only works with rettype="xml" at present
#'@seealso \code{\link[httr]{config}} for available configs
#'@return character string containing the file created
#
#' @examples
#' 
#' katipo <- "Latrodectus katipo[Organism]"
#' katipo_search <- entrez_search(db="nuccore", term=katipo)
#' kaitpo_seqs <- entrez_fetch(db="nuccore", id=katipo_search$ids, rettype="fasta")
#'

entrez_fetch <- function(db, rettype, retmode="text", parsed=FALSE,
                         config=NULL, ...){
    if(parsed){
        if(retmode != "text"){
          msg <- paste("Can't parse records of type", rettype)
          stop(msg)
        }
    }
    records <- make_entrez_query("efetch", db=db, rettype=rettype,
                                 config=config, ..., 
                                 require_one_of = c("id", "WebEnv"))
    #NCBI limits requests to three per second
    Sys.sleep(0.33)
    if(parsed){
        #At the moment, this is just a long-winded way to call
        #XML::xmlTreeParse, but we already use this approach to parse
        #esummaries,and this is more flexible if NCBI starts sharing more
        #records in JSON.
        return(parse_response(records, rettype))
    }
    records
}

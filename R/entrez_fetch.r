#' Download data from NCBI databases
#'
#' A set of unique identifiers mush be specified with either the \code{db}
#' argument (which directly specifies the IDs as a numeric or character vector)
#' or a \code{web_history} object as returned by 
#' \code{\link{entrez_link}}, \code{\link{entrez_search}} or 
#' \code{\link{entrez_post}}. See Table 1 in the linked reference for the set of 
#' formats available for each database.
#'
#' 
#'
#'@export
#'@param db character, name of the database to use
#'@param id vector (numeric or character), unique ID(s) for records in database \code{db} 
#'@param web_history, a web_history object 
#'@param rettype character, format in which to get data (eg, fasta, xml...)
#'@param retmode character, mode in which to receive data, defaults to 'text'
#'@param config vector, httr configuration options passed to httr::GET
#'@param \dots character, additional terms to add to the request, see NCBI
#'documentation linked to in references for a complete list
#'@references \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/#_chapter4_EFetch_} 
#'@param parsed boolean should entrez_fetch attempt to parse the resulting 
#' file. Only works with rettype="xml" at present
#'@seealso \code{\link[httr]{config}} for available configs
#'@return character string containing the file created
#'@return XMLInternalDocument a parsed XML document if parsed=TRUE and
#'rettype='xml'
#
#' @examples
#' 
#' katipo <- "Latrodectus katipo[Organism]"
#' katipo_search <- entrez_search(db="nuccore", term=katipo)
#' kaitpo_seqs <- entrez_fetch(db="nuccore", id=katipo_search$ids, rettype="fasta")
#'

entrez_fetch <- function(db, id=NULL, web_history=NULL, rettype, retmode="text", parsed=FALSE,
                         config=NULL, ...){
    identifiers <- id_or_webenv()
    if(parsed){
        if(rettype != "xml"){            
          msg <- paste("Can't parse records of type", rettype)
          stop(msg)
        }
    }
    args <- c(list("efetch", db=db, rettype=rettype, config=config, ...), identifiers) 
    records <- do.call(make_entrez_query, args) 
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

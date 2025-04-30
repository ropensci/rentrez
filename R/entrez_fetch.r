#' Download data from NCBI databases
#'
#' Pass unique identifiers to an NCBI database and receive data files in a
#' variety of formats.
#
#' A set of unique identifiers must be specified with either the \code{db}
#' argument (which directly specifies the IDs as a numeric or character vector)
#' or a \code{web_history} object as returned by 
#' \code{\link{entrez_link}}, \code{\link{entrez_search}} or 
#' \code{\link{entrez_post}}. 
#' 
#' The format for returned records is set by that arguments \code{rettype} (for
#' a particular format) and \code{retmode} for a general format (JSON, XML text
#' etc). See  \href{https://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.T._valid_values_of__retmode_and/}{Table 1} 
#' in the linked reference for the set of 
#' formats available for each database. In particular, note that sequence
#' databases (nuccore, protein and their relatives) use specific format names
#' (eg "native", "ipg") for different flavours of xml.
#'
#' For the most part, this function returns a character vector containing the 
#' fetched records. For XML records (including 'native', 'ipg', 'gbc' sequence
#' records), setting \code{parsed} to \code{TRUE} will return an
#' \code{XMLInternalDocument},
#'
#'@export
#'@param db character, name of the database to use
#'@param id vector (numeric or character), unique ID(s) for records in database
#'\code{db}. In the case of sequence databases these IDs can take form of an
#' NCBI accession followed by a version number (eg AF123456.1 or AF123456.2).
#'@param web_history, a web_history object 
#'@param rettype character, format in which to get data (eg, fasta, xml...)
#'@param retmode character, mode in which to receive data, defaults to an empty
#'string (corresponding to the default mode for rettype).
#'@param config vector, httr configuration options passed to httr::GET
#'@param \dots character, additional terms to add to the request, see NCBI
#'documentation linked to in references for a complete list
#'@references \url{https://www.ncbi.nlm.nih.gov/books/NBK25499/#_chapter4_EFetch_} 
#'@param parsed boolean should entrez_fetch attempt to parse the resulting 
#' file. Only works with xml records (including those with rettypes other than
#' "xml") at present
#'@seealso \code{\link[httr]{config}} for available '\code{httr}` configs
#'@return character string containing the file created
#'@return XMLInternalDocument a parsed XML document if parsed=TRUE and
#'rettype is a flavour of XML.
#
#' @examples
#' \dontrun{
#' katipo <- "Latrodectus katipo[Organism]"
#' katipo_search <- entrez_search(db="nuccore", term=katipo)
#' kaitpo_seqs <- entrez_fetch(db="nuccore", id=katipo_search$ids, rettype="fasta")
#' #xml
#' kaitpo_seqs <- entrez_fetch(db="nuccore", id=katipo_search$ids, rettype="native")
#'}

entrez_fetch <- function(db, id=NULL, web_history=NULL, rettype, retmode="", parsed=FALSE,
                         config=NULL, ...){
    identifiers <- id_or_webenv()
    if(parsed){
        if(!is_xml_record(rettype, retmode)){            
          msg <- paste("At present, entrez_fetch can only parse XML records, got", rettype)
          stop(msg)
        }
    }
    args <- c(list("efetch", db=db, rettype=rettype, config=config, retmode=retmode, ...), identifiers) 
    records <- do.call(make_entrez_query, args) 
    if(parsed){
        #At the moment, this is just a long-winded way to call
        #XML::xmlTreeParse, but we already use this approach to parse
        #esummaries,and this is more flexible if NCBI starts sharing more
        #records in JSON.
        return(parse_response(records, rettype))
    }
    records
}

is_xml_record <- function(rettype, retmode){
    if(rettype %in% c("xml", "native", "gpc","ipg")){
        return(TRUE)
    }
    retmode == "xml"
}


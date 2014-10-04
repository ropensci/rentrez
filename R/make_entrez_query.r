#Create a URL for the EUtils API. 
#
# This function is usde by all the API-querying functions in rentrez to build
# the appropriate url. Required arguments for each rentrez are handled in each
# function. Those arguments that either ID(s) or are WebEnv cookie can be set
# by passing a string or two argument names to `make_entrez_query`
#
#
# efetch_url <- make_entrez_query("efetch", require_one_of=c("id", "WebEnv"), 
#                                 id=c(23310964,23310965), db="pubmed",
#                                 rettype="xml")
#


make_entrez_query <- function(util, 
                              require_one_of=NULL,
                              config,
                              ...){
    args <- list(..., emails=entrez_email, tool=entrez_tool)
    arg_names <- names(args)
    if(length(require_one_of) > 1 ){
        if(!sum(require_one_of %in% arg_names)==1){
            msg <- paste("Function requires either", require_one_of[1], "or",
                         require_one_of[2], "to be set as arguments\n")
            stop(msg)
        }
    }
    
    if("id" %in% arg_names){
        args$id <- paste(args$id, collapse=",")      
    }
    uri <- paste0("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/", util, ".fcgi?")
    response <- httr::GET(uri, query=args, config= config)
    httr::stop_for_status(response)
    return(httr::content(response, as="text"))
}

parse_respone <- function(x, type){
    res <- switch(type, 
            "json" = jsonlite::fromJSON(x),
            "xml"  = xmlTreeParse(x, useInternalNodes=TRUE)
    )
    return(res)
}






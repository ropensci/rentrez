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
                              full_response=FALSE, 
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
    response <- httr::GET(uri, query=args)
    stop_for_status(response)
    if(full_response){
        return(response)
    }
    else return(httr::content(response, as="text"))

}
#make_entrez_query <- function(util, require_one_of=NULL, ...){
#    args <- list(..., email=entrez_email, tool=entrez_tool)
#    arg_names <- names(args)
#    
#    if(length(require_one_of) > 1 ){
#        if(!any(require_one_of %in% arg_names)){
#            msg <- paste("Function requires either", require_one_of[1], "or",
#                         require_one_of[2], "to be set as arguments\n")
#            stop(msg)
#        }
#    }
#    
#    if("id" %in% arg_names){
#        args[["id"]] = paste(args[["id"]], collapse=",")
#      
#    }
#    base_url <- paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/", util, ".fcgi?", sep="")
#    url_args <- paste(paste(arg_names, args, sep="="), collapse="&")
#    query <- paste(base_url, url_args, sep="&")
#    return(query)
#}
#    
  

#'
#'
#'
#'

#
#Create a URL for the EUtils API. 
#
# This function is usde by all the API-querying functions in rentrez to build the appropriate
# url. It is not exported by rentrez
#

make_entrez_query <- function(util, required_args=NULL, require_one_of=NULL, ...){
    args <- c(..., email=entrez_email, tool=entrez_tool)
    arg_names <- names(args)
    if(length(required_args) > 0){
        if(!all(required_args %in% arg_names)){
            msg <-paste("Not all required arguments (", required_args, 
                        ") present\n")
            stop(msg)
        }
    }
    if(length(require_one_of) > 1 ){
        if(!any(require_one_of %in% arg_names)){
            msg <- paste("Function requires either", require_one_of[1], "or",
                         require_one_of[2], "to be set as arguments\n")
            stop(msg)
        }
    }
    
    if("ids" %in% arg_names){
        args["ids"] = paste(args["ids"], collapse=",")
    }
    base_url <- paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/", util, ".fcgi?", sep="")
    url_args <- paste(paste(arg_names, args, sep="="), collapse="&")
    query <- paste(base_url, url_args, sep="&")
    return(query)
}
    
  

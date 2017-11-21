# Handle NCBI API keys



#' Set the ENTREZ_KEY variable to be used by all rentrez functions
#'
#' The NCBI allows users to acess more records (10 per second) if they 
#' register for and use an API key. This function allows users to set this key
#' for all calls to rentrez functions during a particular R session. See the
#' vignette section "Making use of API keys" for a detailed description.
#'@export
#'@param key characater. Value to set ENTREZ_KEY to.
#
set_entrez_key <- function(key){
    Sys.setenv(ENTREZ_KEY=key)        
}

#internal function, used to test existance of key.
is_entrez_key_set <- function(){
    !identical(Sys.getenv('ENTREZ_KEY'), "")
}


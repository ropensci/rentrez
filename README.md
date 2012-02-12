#rentrez

rentrez provides functions that work with the [NCBI eutils](http://www.ncbi.nlm.nih.gov/books/NBK25500/) 
to search or download data from various NCBI databases. 

The package is _not_ ready for prime time yet, but a few functions are 
implemented. An example usage might go like this

        >library(rentrez)
        >entrez.email <- "david.winter@gmail.com"
        #Find 5 datasets from the Popset database that include snails
        >search <- entrez_search("popset", "Gastropoda[Organism]", retmax=5)
        #download all datasets as one big Genbank file
        >records <- entrez_fetch("popset", search$ids, file_format="gb")

I've only just started on this - and welcome feedback from anyone that might use
a library like this.

        

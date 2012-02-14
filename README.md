#rentrez

rentrez provides functions that work with the [NCBI eutils](http://www.ncbi.nlm.nih.gov/books/NBK25500/) 
to search or download data from various NCBI databases. 

The package is _not_ ready for prime time yet, but a few functions are 
implemented. An example usage might go like this. 

Start by searching for papers on _Durvillaea_ bullkelp, and retreive a 
a particular paper as xml:

        >library(rentrez)
        >
        > entrez_email <- "david.winter@gmail.com"
        > kelp_search <- entrez_search(db="pubmed", "Durvillaea")
        > kelp_paper <- entrez_fetch(db="pubmed", kelp_search$ids[4], file_format="xml")

There are no functions to deal with the xml version of a paper at present, but 
if you know `xpath` you can get some info:
        
        > xml_paper <- xmlTreeParse(kelp_paper, useInternalNodes=TRUE)
        > xpathSApply(xml_paper, "//ArticleTitle", xmlValue)
        > xpathSApply(xml_paper, "//ArticleId[@IdType='doi']", xmlValue)

Find what other resources NCBI for this paper, and download all the nucleotide
sequences as one big fasta file:

        > kelp_data_sources <- entrez_link(db="all", ids=kelp_search$ids[4], dbfrom="pubmed")
        > str(kelp_data_sources)
        > records <- entrez_fetch(db="nuccore", ids=kelp_data_sources$pubmed_nuccore, file_format="fasta")
        > write(records, "records.fasta")


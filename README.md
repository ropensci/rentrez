#rentrez

rentrez provides functions that work with the [NCBI eutils](http://www.ncbi.nlm.nih.gov/books/NBK25500/) 
to search or download data from various NCBI databases. 

The package hasn't been thoroughly tested yet, but the functions for each of 
the Eutils functions are implimented. If you try the package and find bugs
please let me know.

##Install

For now, either download the archive above and install using `$R CMD INSTALL`
or use Hadley Wickham's [devtools](https://github.com/hadley/devtools):
     
         > library(devtools)
         > install_github("rentrez", "dwinter")

##Examples

To see how the package works, let's look at a couple of possible uses of the 
library

###Getting data from that great paper you've just read

Let's say I've just read a paper on the evolution of Hox genes,
[Di-Poi _et al_. (2010)](dx.doi.org/10.1038/nature08789), and I want to get the
data required to replicate their results. First, I need the unique ID for this
paper in pubmed (the PMID). Annoyingly, many journals don't give PMIDS for their
papers, but we can use `entrez_search` to find the paper using the doi field:

  
        > hox_paper <- entrez_search(db="pubmed", term="10.1038/nature08789[doi]")
        > (hox_pmid <- Anolis_paper$ids)
        # [1] 20203609

Now, what sorts of data are avaliable from other NCBI database for this paper?

        > hox_data <- entrez_link(db="all", ids=hox_pmid, dbfrom="pubmed")
        > str(hox_data)
        #List of 11
        # $ pubmed_nuccore            : chr [1:32] "290760437"  ...
        # $ pubmed_pmc_refs           : chr [1:11] "3218823" ...
        # $ pubmed_protein            : chr [1:49] "290760438" ...
        # $ pubmed_pubmed             : chr [1:128] "20203609" ...
        # $ pubmed_pubmed_citedin     : chr [1:10] "22016857"  ...
        # $ pubmed_pubmed_combined    : chr [1:6] "20203609" " ...
        # $ pubmed_pubmed_five        : chr [1:6] "20203609" ...
        # $ pubmed_pubmed_reviews     : chr [1:31] "20203609"  ...
        # $ pubmed_pubmed_reviews_five: chr [1:6] "20203609"  ...
        # $ pubmed_taxonomy_entrez    : chr [1:14] "742354"  ...
        # $ file                      :Classes 'XMLInternalDocument'...'

Now we can use `entrez_fetch` for the protein sequences as genbank files:
 
        > hox_proteins <- entrez_fetch(db="protein", ids=hox_data$pubmed_protein, file_format="gb")



###Retreiving datasets associated a particular organism.

I like spiders. So let's say I want to learn a little more about New Zealand's
endemic "black widow" the katipo. Specifically, in the past the katipo has 
been split into two species, can we make a phylogeny to test this idea?

The first step here is to use the function `entrez_search` to find datasets
that include katipo sequences.

        > library(rentrez)
        > katipo_search <- entrez_search(db="popset", term="Latrodectus katipo[Organism]")
        > katipo_search$count
        # [1] 6

In this search `count` is the total number of hits returned for the search term.
We can use `entrez_summary` to learn a little about these datasets. Because 
different databases give differnt xml files, `entrez_summary` returns an xml 
file for you to further process. In this case, a little xpath can tell us about
each dataset.

        > summaries <- entrez_summary(db="popset", ids=katipo_search$ids)
        > xpathSApply(summaries, "//Item[@Name='Title']", xmlValue)
        #[1] "Latrodectus katipo 18S ribosomal RNA gene ..."
        #[2] "Latrodectus katipo cytochrome oxidase subunit 1 (COI)..."
        #[3] "Latrodectus 18S ribosomal RNA gene..."
        #[4] "Latrodectus cytochrome 1 oxidase subunit 1 (COI)...""
        #[5] "Latrodectus tRNA-Leu (trnL) gene ... ""                                               
        #[6] "Theridiidae cytochrome oxidase subunit I (COI) gene ..."

Let's just get the two mitochondrial loci (COI and trnL), using `entrez_fetch`:

        > COI_ids <- katipo_search$ids[c(2,6)]
        > trnL_ids <- katipo_search$ids[5]
        > COI <- entrez_fetch(db="popset", ids=COI_ids, file_format="fasta")
        > trnL <- entrez_fetch(db="popset", ids=trnL_ids, file_format="fasta")

The "fetched" results are fasta formatted characters, which can be written
to disk easily:

        > write(COI, "Test/COI.fasta")      
        > write(trnL, "Test/trnL.fasta")

Once you've got the sequences you can do what you want with them, but I wanted 
a phylogeny so let's do that with ape:

        > library(ape)
        > coi <- read.dna("Test/COI.fasta", "fasta")
        > coi_aligned <- clustal(coi)
        > tree <- nj(dist.dna(coi_aligned))


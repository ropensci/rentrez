#rentrez

rentrez provides functions that work with the [NCBI eutils](http://www.ncbi.nlm.nih.gov/books/NBK25500/) 
to search or download data from various NCBI databases. 

The package hasn't been thoroughly tested yet, but the functions for each of 
the Eutils functions are implimented. If you try the package and find bugs
please let me know.

##Install

For now, either download the archive above and install using `$R CMD INSTALL`
or use Hadley Wickham's [devtools](https://github.com/hadley/devtools):

```r     
library(devtools)
install_github("rentrez", "ropensci")
```


##The Eutils API

`rentrez` presumes you already know your way around the Eutils' API, [which is well 
documented](http://www.ncbi.nlm.nih.gov/books/NBK25500/). Make sure you read the
documentation, and in particular, be aware of the NCBI's usage policies and try to
limit very large requests to off peak (USA) times. 

The functions in `rentrez` are designed to create URLs in the form required by 
the api, fetch the file and parse information from it. Specific examples below illustrate
how the functions work.

##Examples

To see how the package works, let's look at a couple of possible uses of the 
library

###Getting data from that great paper you've just read

Let's say I've just read a paper on the evolution of Hox genes,
[Di-Poi _et al_. (2010)](dx.doi.org/10.1038/nature08789), and I want to get the
data required to replicate their results. First, I need the unique ID for this
paper in pubmed (the PMID). Annoyingly, many journals don't give PMIDS for their
papers, but we can use `entrez_search` to find the paper using the doi field:

```r  
hox_paper <- entrez_search(db="pubmed", term="10.1038/nature08789[doi]")
(hox_pmid <- hox_paper$ids)
        # [1] 20203609
```

Now, what sorts of data are avaliable from other NCBI database for this paper?

```r
hox_data <- entrez_link(db="all", ids=hox_pmid, dbfrom="pubmed")
str(hox_data)
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
```

Each of the character vectors in this object contain unique IDS for records in
the named databases. These functions try to make the most useful bits of the 
returned files available to users, but they also return the original file in case 
you want to dive into the XML yourself.

In this case we'll get the protein sequences as genbank files, using ' `entrez_fetch`:
 
```r
hox_proteins <- entrez_fetch(db="protein", ids=hox_data$pubmed_protein, file_format="gb")
```

###Retreiving datasets associated a particular organism.

I like spiders. So let's say I want to learn a little more about New Zealand's
endemic "black widow" the katipo. Specifically, in the past the katipo has 
been split into two species, can we make a phylogeny to test this idea?

The first step here is to use the function `entrez_search` to find datasets
that include katipo sequences. The `popset` database has sequences arising from
phylogenetic or population-level studies, so let's start there.

```r
library(rentrez)
katipo_search <- entrez_search(db="popset", term="Latrodectus katipo[Organism]")
katipo_search$count
        # [1] 6
```        

In this search `count` is the total number of hits returned for the search term.
We can use `entrez_summary` to learn a little about these datasets. Because 
different databases give differnt xml files, `entrez_summary` returns an xml 
file for you to further process. In this case, a little xpath can tell us about
each dataset.

```r
summaries <- entrez_summary(db="popset", ids=katipo_search$ids)
xpathSApply(summaries, "//Item[@Name='Title']", xmlValue)
        #[1] "Latrodectus katipo 18S ribosomal RNA gene ..."
        #[2] "Latrodectus katipo cytochrome oxidase subunit 1 (COI)..."
        #[3] "Latrodectus 18S ribosomal RNA gene..."
        #[4] "Latrodectus cytochrome 1 oxidase subunit 1 (COI)...""
        #[5] "Latrodectus tRNA-Leu (trnL) gene ... ""                                               
        #[6] "Theridiidae cytochrome oxidase subunit I (COI) gene ..."
```

Let's just get the two mitochondrial loci (COI and trnL), using `entrez_fetch`:

```r
COI_ids <- katipo_search$ids[c(2,6)]
trnL_ids <- katipo_search$ids[5]
COI <- entrez_fetch(db="popset", ids=COI_ids, file_format="fasta")
trnL <- entrez_fetch(db="popset", ids=trnL_ids, file_format="fasta")
```

The "fetched" results are fasta formatted characters, which can be written
to disk easily:

```r
write(COI, "Test/COI.fasta")      
write(trnL, "Test/trnL.fasta")
```

Once you've got the sequences you can do what you want with them, but I wanted 
a phylogeny so let's do that with ape:

```r
library(ape)
coi <- read.dna("Test/COI.fasta", "fasta")
coi_aligned <- clustal(coi)
tree <- nj(dist.dna(coi_aligned))
```

### WebEnv and big queries

The NCBI provides search history features, which can be useful for dealing with alrge lists of IDs (which will not fit in a single URL) or repeated searches. As an example, we will go searching for COI sequences from all the land snail (Stylommatophora) species we can find in the nucleotide database:
```r	
library(rentrez)
snail_search <- entrez_search(db="nuccore", "Gastropoda[Organism] AND COI[Gene]", retmax=200, usehistory="y")
```
       
Because we set usehistory to "y" the `snail_search` object contains a unique ID for the search (`WebEnv`) and the particular query in that search history (`QueryKey`). Instead of using the 200 ids we turned up to make a new URL and fetch the sequences we can use the webhistory features. 

```r
cookie <- snail_search$WebEnv
qk <- snail_search$QueryKey
snail_coi <- entrez_fetch(db="nuccore", WebEnv=cookie, query_key=qk, file_format="fasta")
```

###Trendy topics in genetics

This is one is a little more trivial, but you can also use entrez to search pubmed and
the EUtils API allows you to limit searches by the year in which the paper was published.
That gives is a chance to find the trendiest -omics going around (this has quite a lot
of repeated searching, so it you want to run your own version be sure to do it
in off peak times). 

Let's start by making a function that finds the number of records matching a given
search term for each of several years (using the `mindate` and `maxdate` terms from
the Eutils API):

```r
library(rentrez)
papers_by_year <- function(years, search_term){
            return(sapply(years, function(y) entrez_search(db="pubmed",term=search_term, mindate=y, maxdate=y, retmax=0)$count))
        }
```        

With that we can fetch the data for earch term and, by searching with no term, 
find the total number of papers published in each year:

        
```r
years <- 1990:2011
total_papers <- papers_by_year(years, "")
omics <- c("genomic", "epigenomic", "metagenomic", "proteomic", "transcriptomic", "pharmacogenomic", "connectomic" )
trend_data <- sapply(omics, function(t) papers_by_year(years, t))
trend_props <- trend_data/total_papers
```
        
That's the data, let's plot it:

```r
library(reshape)
library(ggplot2)
trend_df <- melt(as.data.frame(trend_props), id.vars="years")
p <- ggplot(trend_df, aes(years, value, colour=variable))
png("trendy.png", width=500, height=250)
p + geom_line(size=1) + scale_y_log10("number of papers")
dev.off()
```


Giving us... well this:

![](http://i.imgur.com/LDpP1.png)




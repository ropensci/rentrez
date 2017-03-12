<!-- 

Sometimes rentrez will throw obscure error messages because the NCBI's webservers
are down or otherwise not behaving as they are meant to. Before reporting issues
about errors thrown while contacting an NCBI database (search, fetch, summary,
link...) you can test the NCBI is responding to requests by pasting the
following URLS into a web browser.

https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=snp&id=6060535&retmote=rsr
https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=rentrez

They should give you a plain text file and a small XML file respectively. If you
receive errors when trying to aces the URLs it is likely they the NCBI is
having an intermittent problem, try your request again in a few minutes.

If those pages load as expected then please file your issue. We appreciate user
feedback and will do our best to help. 
-->

# rentrez (development version)

## BUG FIXES

* `entrez_link()` now reports what NCBI said when a reply carries no usable
  content. A command NCBI will not serve comes back as HTTP 200 with an
  `<ERROR>` inside the LinkSet, and five of the nine `cmd` parsers ran on into
  a subscript or a names error that named no cause (#215).

* `entrez_link(cmd = "llinks")` and its siblings return an empty set for an ID
  that has no linkouts, where they used to fail. An ID with nothing to link is
  an answer rather than a failure, so only a reply carrying an NCBI error
  stops now.

* `entrez_search(rettype = "count")` returns the count, and the result prints.
  A count-only reply carries a `Count` and nothing else, which the parser read
  as missing nodes and `print()` read as a missing query translation (#153).
  Asking for `use_history` alongside it now warns and attaches no web history,
  where before it failed on the same subscript. NCBI sends no QueryKey or
  WebEnv for a count-only search, so there is none to attach.

* `entrez_link(by_id = TRUE)` returns a list for a single ID, as it already did
  for several, and no longer warns that the ID was invalid when it was not
  (#175).

* `entrez_search()` reports what NCBI said when a search comes back with no
  result, rather than failing on a subscript. NCBI answers a bad database name
  with an ordinary HTTP 200 whose body carries the reason, and the JSON path
  did not check for it at all, so `retmode = "json"` returned a record built
  from missing pieces that then failed when printed (#187).

* Requests that send more than 200 IDs now use POST, as intended. The check
  that chooses POST over GET ran after the IDs had been collapsed into a
  single string, so it never matched and every request used GET
  (#174, thanks @allenbaron).

* `entrez_fetch(parsed = TRUE)` parses both `rettype = "gbc"` and
  `rettype = "gpc"`, the INSDSeq XML formats for nucleotide and protein
  records. The check listed only `"gpc"` and the parser handled only `"gbc"`,
  so a `"gbc"` request stopped with a message calling it unparseable while a
  `"gpc"` request got past the check and came back as text (#228).

## MINOR IMPROVEMENTS

* HTTP failures name the query that caused them, with the API key removed. The
  key reaches these messages two ways, in the query string whenever one is set
  and quoted back by NCBI when it rejects one, and these messages get pasted
  into bug reports (#159).

* `?entrez_search` names xml as the default for `retmode`, which is what the
  function has always used. The page said json, and added that the choice makes
  no difference in most cases, which the Value section already covers in more
  detail (#224).

* Removed stray characters from two error messages (#211).

# rentrez 1.2.4

Maintenance release to meet CRAN policies.

* S3 methods are now registered with `@exportS3Method` instead of being
  exported directly.
* Documentation links to the XML package are now fully qualified.

# rentrez 1.2.3

Maintenance release, mostly to prevent issues with rate-limiting errors when the
package is tested in CRAN. 

* The sleep commands for rate-limiting are slightly increased

* As of this release, the vignette is NOT build by default (to avoid issues with
  automated tests on CRAN). This will not affect most users, but a developers
  may want to read a wiki page describing how to build the vignette:

https://github.com/ropensci/rentrez/wiki/Building-the-rentrez-tutorial-vignette.



# rentrez 1.2.2

Maintenance release containing a number of bug fixes. 

* A fix #127 forces curl to use http/1.1, as NCBI produces errors when sing http/2
  (thanks @hammer for bug report and fix)
* fix #131 stops pubmed records with reference sections production > 1
  value for the 'pmid' field. Thanks @agbarnett for the report.
* Added clarification around the expectd input for `extract_from_esummary` to
  the docs.

# rentrez 1.2.1


Bug fix release. Thanks @gmbecker for pointing out a problem with the way
retmode was being used in entrez_fetch (PR #121). 

Also updates documentation to make this clear.
Removes depricated verbs from testthat code.


# rentrez 1.2.0


rentrez updated to reflect NCBIs new API policy, allowing more requests from
users with registered keys. (Issues #115 -- #117).

CITATION updated to reflect publication in The R Journal.

Minor changes
   * clarification of search syntax in docs (issue #120)
   * ORCID ids added for all authors (issue #118)
   

# rentrez 1.1.0


As of this release, rentrez will use httr::POST when sending > 200 ids to the
NCBI. This should make working with large ID sets easier (thanks to the NCBI for
supporing the POST methods, Reed Cartwright and Chris Stubben for pushing me on
issue #89). 

Other minor changes:
    * Pass on error messages from NCBI when too many records are requested from 
      `entrez_summary` (Issue #106)
    * Useful error message when trying to send an empty ID set to NCBI (Issue #107)

# rentrez 1.0.4

Update to documentation and tests to accommodate versioned accessions now
available from NCBI (see ?entrez_fetch and the vignette)

# rentrez 1.0.3

Update to only use https
    * NCBI is goinh all https, rentrez will only use https from now on.
    * Added links to repo/bug reporting to DESCRIPTION 
    * Documented changes to sequence database XML records
    * Allow automatic parsing of XML flavours


# rentrez 1.0.2

Bug fix release
    * Tests now work with testthat 1.0.0 
    * All calls to ncbi specify encoding is UTF-8 (saving error messages)
    * HTTP Error codes associated with large requests now give the user a hint
      to check out the documentation for web-history features

# rentrez 1.0.1

Bug fix release
    * Properly format "by_id" mode URLS (bug exposed by httr 1.0.1)
    * Handle case in which some IDs passed to "by_id" mode are invalide (thanks
      Zachary Foster for report)
    * Documentation updated to reflect OMIM->SNP links no longer possible
    * Use Rmarkdown (not knitr) as vignette builder
    * Return NCBI error messages are text when they exist

# rentrez 1.0.0

    * new function extract_from_esummary() for extracting like-named elements
      from a list esummary records (e.g. get all "Title" fields from a list of 
      PubMed esummaries)
    * Support for `cmd` option in entrez_link (breaks backward compatibility)
        * Allows discovery of external links from and use of web_history
        * New helper function linkout_urls to get URLs form external links
    * Support for 'by_id' mode for entrez_link. Pass a vector of IDs to
      entrez_link, (optionally) get a list on elink objects back (one per ID)
    * New web_history object makes using NCBI Web History features easier
    * All of these changes documented in new vignette
    * Han Guangchun added as contributor for his pull requests 
    * New tests, minor bug fixes and extended documentation



# rentrez 0.4.1

* Bug fix: The example for entrez_summary contained a typo which made it fail
  (being wrapped in dontest this hadn't previously shown up).

# rentrez 0.4

 * entrez_summary now fetches 'version 2.0' esummary records from NCBI
     * This change may break some scripts. In particular, the names of some
       elements in esummary records have changed. Broken scripts shold produce a
       helpful error message, and using entrez_summary(..., version="1.0")
       should fix it. More details are given in the help to entrez_summary. 
     * When version 2.0 records are requested entrez_summary fetches the json
       record.
 * New helper functions for einfo Eutil
    * entrez_dbs() lists avaliable databases.
    * entrez_db_summary() gets summary information about a given database.  
    * entrez_db_links() lists databases against which a given db's records might
      be cross referenced.
    * entrez_db_searchable() lists search terms avaliable for a given database.
 * Nicer print functions for search and summary objects
 * New dependancy on jsonlite for handling json records.
 * Bunch of bugs squashed and typos cleaned up

# rentrez 0.3.1

    * Squashed a bug in the vignette which wrote to users $HOME

# rentrez 0.3

    * using httr to handle HTTP GETs and some url building
    * parsing for esummary parsing for clinvar database
    * Scott Chamberlain added as contributer for above
    * Pubmed parser handles multi-record files
    * html vignette included

# rentrez 0.2.4

    * minor release to fix bug in esummary parsing

# rentrez 0.2.3

    * Edited license/description to meet CRAN requiremens
    * Added sentence to description to summarise the package


# rentrez 0.2.2


    * Parsing of esummary xmls is now much nicer. 
    * S3 items to represent most results
    * Tests to cover all functions


# rentrez 0.1.1

    * First release on CRAN + now part of ROpenSci
    * Functions cover the whole EUtils API

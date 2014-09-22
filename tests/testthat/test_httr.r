context("httr option passing")
    #most config options  don't produce capture-able output, so instead
    # we will test if we raise an error when we us a non-existant proxy to
    # connect to the internet
    test_that("httr config options can be passed to rentrez functions",{
        expect_error(entrez_search(db="popset", 
                                   term="test",
                                   config=use_proxy(url="0.0.0.0", port=80 )))        
})

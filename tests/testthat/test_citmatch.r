context("Cite matching")
test_that("Citation matching works",{
     ex_cites <- c("proc natl acad sci u s a|1991|88|3248|mann bj|test1|",
                   "science|1987|235|182|palmenberg ac|test2|")
     res <- entrez_citmatch(ex_cites)
     expect_that(res, is_a("character"))
     expect_equal(res,  c("2014248", "3026048"))
     expect_warning(entrez_citmatch(c("some|nonsense|", ex_cites)))
})

      


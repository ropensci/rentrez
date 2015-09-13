library("testthat")

#All of the tests rely on the API existing and behaving as documented. However,
#the API occasionally falls over or stops working which lets to errors on CRAN.
#Because we use travis CI we will hear about any test failures as soon as they
#happen. So, let's skill all tests on CRAN:

if(identical(Sys.getenv("NOT_CRAN"), "true")){
    test_check("rentrez")
}

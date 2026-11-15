library("testthat")

#Most test files call the live NCBI API and start with skip_on_cran(), so CRAN
#and r-universe, which leave NOT_CRAN unset, run only the tests that need no
#network. devtools::check() and the r-lib GitHub Actions set NOT_CRAN=true and
#run the whole suite (#221).
test_check("rentrez")

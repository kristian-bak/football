
## Document
devtools::document()

## Load all functions
devtools::load_all()

## Init renv
renv::init()

## Status
renv::status()

## Add package
usethis::use_package("dplyr")

## Add test
usethis::use_test("load_seasons")

## Code coverage
devtools::test_coverage()

## Add pipe
usethis::use_pipe()


## Document
devtools::document()

## Load all functions
devtools::load_all()

## Run tests
devtools::test()

## Status
renv::status()

## Add package
usethis::use_package("dplyr")

## Add test
usethis::use_test("load_seasons")
usethis::use_test("season_seq")

## Code coverage
devtools::test_coverage()

## Install locally
devtools::install()

## Add pipe
usethis::use_pipe()

## Init renv
renv::init()

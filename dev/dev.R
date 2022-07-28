
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
usethis::use_package("rvest")
usethis::use_package("glue")

## Install github dependency
remotes::install_github(repo = "kristian-bak/kb.utils")

## Add test
usethis::use_test("load_seasons")
usethis::use_test("get_futbin_url")
usethis::use_test("get_league_table")
usethis::use_test("get_minutes_goals_scored")
usethis::use_test("season_seq")

## Code coverage
devtools::test_coverage()

## Install locally
devtools::install()

## Add pipe
usethis::use_pipe()

## Init renv
renv::init()

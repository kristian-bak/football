test_that("Test: get_futbin_url", {

  url <- get_futbin_url(
    year = 20,
    league_name = "Premier League",
    club_name = "Liverpool",
    page_number = 1
  )

  expect_equal(as.character(url), "https://www.futbin.com/20/players?page=1&club=11&league=13")

})

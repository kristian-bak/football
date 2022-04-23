test_that("Test: season_seq", {

  res <- season_seq(from = 20, to = 22)

  expect_equal(res, c("2021", "2122"))

})

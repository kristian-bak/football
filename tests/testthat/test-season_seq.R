test_that("Test: season_seq", {

  res <- season_seq(from = 20, to = 22)

  out <- kb.utils::catch_error(expr = season_seq(from = "20", to = "22"))

  expect_false(is.null(out$error))

})

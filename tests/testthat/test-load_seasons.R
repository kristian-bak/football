test_that("Test: load_seasons + get_team_names", {

  data <- load_seasons(from = 20, to = 22)

  expect_equal(class(data)[1], "tbl_df")

  expect_equal(
    data %>%
      dplyr::pull(Season) %>%
      unique(),
    c("2021", "2122")
  )

  expect_true(
    nrow(data) > 380
  )

  expect_true(
    "FTHG" %in% names(data)
  )

  team_names <- get_team_names(data = data)

  expect_true("Liverpool" %in% team_names)

})

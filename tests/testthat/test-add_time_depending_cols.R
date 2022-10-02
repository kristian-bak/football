test_that("Test: add_time_depending_cols", {

  data <- load_seasons(league = "Premier League", from = 21, to = 22)

  data_out <- data %>%
    head(20) %>%
    add_time_depending_cols()

  df <- data_out %>%
    dplyr::select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTP, ATP, HTF, ATF, HTGS, ATGS, HTGC, ATGC, HTGD, ATGD)

  expect_false(
    is.na(df) %>%
      any()
  )

  expect_equal(
    df %>%
      tail(1),
    dplyr::tibble(
      Date = as.Date("2021-08-23"),
      HomeTeam = "West Ham",
      AwayTeam = "Leicester",
      FTHG = 4L,
      FTAG = 1L,
      FTR = "H",
      HTP = 3,
      ATP = 3,
      HTF = 3,
      ATF = 3,
      HTGS = 4,
      ATGS = 1,
      HTGC = 2,
      ATGC = 0,
      HTGD = 2,
      ATGD = 1)
  )

})

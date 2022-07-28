test_that("Test: get_league_table", {

  out <- get_league_table(league = "premier-league", season = "2021-2022", round = 38)

  expect_equal(class(out)[1], "tbl_df")

  expect_equal(names(out), c("#", "Team", "M.", "W", "D", "L", "goals", "Dif.", "Pt."))

})

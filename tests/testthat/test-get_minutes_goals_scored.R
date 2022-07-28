test_that("multiplication works", {

  df_goals <- get_match_report(
    league = "premier-league",
    season = "2021-2022",
    home_team = "arsenal-fc",
    away_team = "manchester-united"
  )$df_goals

  out <- get_minutes_goals_scored(df_goals)

  expect_equal(out, c(3, 32, 34, 70))

})

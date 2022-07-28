#' Vectorized minutes goals scored
#' @param df tibble with HomeTeam and AwayTeam (see `expand_games_grid`)
#' @return list with minutes goals scored for each game
#'
vectorized_minutes_goals_scored <- function(df) {

  n <- nrow(df)

  data_list <- list()

  for (i in 1:n) {

    data_list[[i]] <- get_match_report(
      league = df$League[i],
      season = df$Season[i],
      home_team = df$HomeTeam[i],
      away_team = df$AwayTeam[i]
    )$df_goals %>%
      get_minutes_goals_scored()

    cat("\r", i, "of", n)
    flush.console()

  }

  return(data_list)

}

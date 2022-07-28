#' Remove acutes
#' @param x character string (typically this function is used with team names)
#'
remove_acutes <- function(x) {
  gsub(pattern = "à", replacement = "a", x = x) %>%
    gsub(pattern = "é", replacement = "e", x = .)
}

#' Modify team name for worldfootball
#' @param x character string (typically this function is used with team names)
#'
modify_team_name_for_worldfootball <- function(x) {
  remove_acutes(x = x) %>%
    tolower() %>%
    gsub(pattern = " ", replacement = "-", x = .)
}

#' Get champions league goal socring minute
#' @param df_games tibble with HomeTeam, AwayTeam, Season and Stage
#'
get_champions_league_goal_scoring_minute <- function(df_games) {

  n <- nrow(df_games)

  list_goals <- list()

  for (i in 1:n) {

    season    <- df_games$Season[i]
    stage     <- df_games$Stage[i]
    home_team <- df_games$HomeTeam[i]
    away_team <- df_games$AwayTeam[i]

    url <- glue::glue("https://www.worldfootball.net/report/champions-league-", {season}, "-", {stage},
                      "-", {home_team}, "-", {away_team}, "/")

    list_goals[[i]] <- url %>%
      rvest::read_html() %>%
      rvest::html_table() %>%
      purrr::pluck(4) %>%
      get_minutes_goals_scored()

    cat("\r", i, "of", n)
    flush.console()

  }

  return(list_goals)

}

#' Get match report
#' @param league league name (for instance "premier-league")
#' @param season season (for insance "2021-2022")
#' @param home_team name of home team (for instance "arsenal-fc")
#' @param away_team name of away team (for instance "manchester-united")
#' @return named list with objects
#' * df_goals: tibble with columns X1 and X2 providing goal scoring and goal scorer information
#' * home_team: tibble with columns X1 (shirt number), X2 (name of player) and X3 (minute of substituion)
#' * away_team: tibble similar to that of home_team
#' * managers: tibble with column X1 and X2 providing information on managers for home and away team
#' * info: tibble with stadium name, number of participants and referees
#'
get_match_report <- function(league, season, home_team, away_team) {

  url <- get_url_for_match_report(
    league = league,
    season = season,
    home_team = home_team,
    away_team = away_team
  )

  webpage <- rvest::read_html(url)

  tables <- webpage %>%
    rvest::html_table(fill = TRUE)

  if (length(tables) == 9) {
    j <- 6
  } else {
    j <- 7
  }

  df_goals     <- tables[[4]]
  home_team    <- tables[[j]]
  away_team    <- tables[[j + 1]]
  managers     <- tables[[j + 2]]
  stadium_info <- tables[[j + 3]]

  out <- list(
    "url"                = url,
    "df_goals"           = df_goals,
    "goalscoring_minute" = get_minutes_goals_scored(df = df_goals),
    "goalscoring_type"   = get_goalscoring_type(df = df_goals),
    "assists"            = get_assists(df = df_goals),
    "home_team"          = home_team,
    "away_team"          = away_team,
    "managers"           = managers,
    "stadium_info"       = stadium_info
  )

  return(out)

}

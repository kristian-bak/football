#' Get url for match report
#' @inheritParams get_match_report
#'
get_url_for_match_report <- function(league, season, home_team, away_team) {

  glue::glue(
  "https://www.worldfootball.net/report/",
  {league}, "-", {season}, "-", {home_team}, "-", {away_team}
  )

}

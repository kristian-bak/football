#' Get league table
#' @inheritParams get_match_report
#' @param round round (integer between 1 and 38) indicating game round (default is 1)
#'
get_league_table <- function(league, season, round = 1) {

  validate_season(season = season)

  country <- get_country_from_league(league = league)

  url <- glue::glue(
    "https://www.worldfootball.net/schedule/", {country},"-", {league}, "-", {season}, "-spieltag/", {round}
  )

  rvest::read_html(url) %>%
    rvest::html_table(fill = TRUE) %>%
    purrr::pluck(4) %>%
    dplyr::select(-2)

}


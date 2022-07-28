#' Get futbin url
#' @param year year (integer), for instance 20
#' @param page_number page number (integer)
#' @return character string
#'
get_futbin_url <- function(year, league_name, club_name, page_number) {

  data <- load_one_season(season = map_year_to_season(year = year))

  league <- map_league_name_to_number(league_name = league_name)
  club   <- map_club_name_to_number(data = data, club_name = club_name)

  glue::glue(
    "https://www.futbin.com/{year}/players?",
    "page={page_number}",
    "&club={club}",
    "&league={league}"
  )

}

#' Map club name to number
#' @param data tibble with historical football results. See `load_seasons`
#' @param club_name character string with club name (for instance "Liverpool")
#' @details The number is found by sorting all club names and picking the index corresponding to `club_name`
#' For this reason `data` argument is needed
#'
map_club_name_to_number <- function(data, club_name) {

  which(club_name == get_team_names(data = data))

}

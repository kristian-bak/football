#' Get team names
#' @param data tibble obtained from `load_seasons`
#' @return character vector with team names in data
#'
get_team_names <- function(data) {

  data %>%
    dplyr::pull(HomeTeam) %>%
    unique() %>%
    sort()

}

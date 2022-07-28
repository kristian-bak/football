#' Get fifa team ratings
#'
get_fifa_team_ratings <- function() {

  "https://www.fifaratings.com/league/english-premier-league" %>%
    rvest::read_html() %>%
    rvest::html_table() %>%
    purrr::pluck(1) %>%
    dplyr::mutate(
      Team = kb.utils::remove_everything_after(pattern = " Prestige", x = Team) %>%
        trimws()
    ) %>%
    dplyr::filter(Team != "") %>%
    dplyr::mutate(Team = map_fifa_team_name_to_uk_data(Team)) %>%
    dplyr::select(-1)

}

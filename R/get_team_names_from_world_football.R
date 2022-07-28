#' Get team names from worldfootball
#' @inheritParams get_match_report
#'
get_team_names_from_world_football <- function(league, season) {

  get_league_table(league = league, season = season) %>%
    dplyr::pull(Team) %>%
    gsub(pattern = "\\&", replacement = "", x = .) %>% ## Brighton has & in its name
    gsub(pattern = "  ", replacement = " ", x = .) %>% ## Removing & gives an extra space
    gsub(pattern = " ", replacement = "-", x = .) %>%  ## Replacing spaces with -
    gsub(pattern = "á", replacement = "a", x = .) %>%  ## Replacing á with a
    gsub(pattern = "é", replacement = "e", x = .) %>%  ## Replacing é with e
    tolower()

}

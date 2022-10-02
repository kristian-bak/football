#' Expand games grid
#' @param team_names charater vector with team names (see `get_team_names_from_world_football`)
#' @param league character string with league name (for instance "premier-league")
#' @param season character string with season(for instance "2021-2022")
#' @return tibble with grid consisting of all games possible from `team_names`
#'
expand_games_grid <- function(team_names, league, season) {

  expand.grid(HomeTeam = team_names, AwayTeam = team_names,
              League = league, Season = season,
              stringsAsFactors = FALSE) %>%
    dplyr::as_tibble() %>%
    dplyr::filter(HomeTeam != AwayTeam)

}

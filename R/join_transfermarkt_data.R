#' Join transfermarkt data
#' @param data tibble with season data (see get_modeling data)
#' @param season integer (for instance 2020 which covers 2021 season)
#'
join_transfermarkt_data <- function(data, season) {

  names_season <- data %>%
    dplyr::pull(Team) %>%
    unique()

  df_transfer <- get_avg_market_value(season = season) %>%
    dplyr::mutate(
      Team = map_transfermarkt_to_season_names(names_transfermarkt = Team, names_season = names_season)
    )

  data %>%
    dplyr::left_join(df_transfer, by = "Team") %>%
    dplyr::left_join(df_transfer %>%
                       dplyr::rename(
                         avg_age_opponent = avg_age,
                         avg_market_value_opponent = avg_market_value,
                         Opponent = Team
                       ), by = "Opponent")

}

#' Join fifa ratings
#' @param data tibble obtained from `load_seasons`
#'
join_fifa_ratings <- function(data) {

  df_fifa_ratings <- get_fifa_team_ratings()

  data %>%
    dplyr::left_join(df_fifa_ratings %>% dplyr::rename_with(.cols = 2:5, ~paste0("Home", .x)), by = c("HomeTeam" = "Team")) %>%
    dplyr::left_join(df_fifa_ratings %>% dplyr::rename_with(.cols = 2:5, ~paste0("Away", .x)), by = c("AwayTeam" = "Team"))



}

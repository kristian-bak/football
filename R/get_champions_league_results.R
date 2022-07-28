#' Mutate and select champions league cols
#' @param data tibble to mutate and select desired columns based on the given stage
#' @param stage character string with stage name
#'
mutate_and_select_champions_league_cols <- function(data, stage) {

  if (grepl("gruppe", stage)) {
    data %>%
      dplyr::mutate(Season = season,
                    Stage = stages[i],
                    Game = NA,
                    HomeTeam = modify_team_name_for_worldfootball(X3),
                    AwayTeam = modify_team_name_for_worldfootball(X5)) %>%
      dplyr::rename(Result = X6) %>%
      dplyr::select(Season, Stage, Game, HomeTeam, AwayTeam, Result)

  } else {
    data %>%
      dplyr::mutate(Season = season,
                  Stage = stages[i],
                  HomeTeam = modify_team_name_for_worldfootball(X2),
                  AwayTeam = modify_team_name_for_worldfootball(X4)) %>%
      dplyr::rename(Game = X1,
                    Result = X5) %>%
      dplyr::select(Season, Stage, Game, HomeTeam, AwayTeam, Result)
  }

}

#' Get champions league results
#' @param season character string with season (for instance "2021-2022")
#' @return tibble with results
#'
get_champions_league_results <- function(season) {

  tournament <- "champions-league"
  stages <- c("halbfinale", "viertelfinale", "achtelfinale",
              paste("gruppe", letters[1:8], sep = "-"))
  n_stages <- length(stages)

  df_list <- list()

  for (i in 1:n_stages) {

    url <- glue::glue("https://www.worldfootball.net/schedule/",
                      {tournament}, "-", {season}, "-", {stages[i]}, "/0/")

    tables <- url %>%
      rvest::read_html() %>%
      rvest::html_table()

    leg1 <- seq(from = 1, to = nrow(df_games) - 3, by = 4)
    leg2 <- leg1 + 1

    df_list[[i]] <- tables[[2]] %>%
      dplyr::slice(c(leg1, leg2)) %>%
      mutate_and_select_champions_league_cols(stage = stages[i])

    cat("\r", i, "of", n_stages)
    flush.console()

  }

  df_out <- do.call("rbind", df_list)

  return(df_out)

}






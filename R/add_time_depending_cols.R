#' Add time depending columns
#' @param data tibble returned from load_seasons
#' @return tible with columns HTGS, HTGC, HTGD, HTF, HTLS, HTP, ATGS, ATGC, ATGD, ATF, ATLS, ATP
#' @details
#' The following time depending columns are added (similar for AT, Away Team):
#' HTGS: Home Team Goal Scored in the season
#' HTGC: Home Team Goals Conceded in the season
#' HTGD: Home Team Goals Difference in the season
#' HTF: Home Team Form (total points in last 5 games)
#' HTP: Home Team Points
#'
#' Note: the columns provide status information prior to the match
add_time_depending_cols <- function(data) {

  n_rows <- nrow(data)

  teams <- data %>%
    dplyr::pull(HomeTeam) %>%
    unique() %>%
    sort()

  n_teams <- length(teams)

  data$HTP  <- rep(as.integer(NA), n_rows)
  data$ATP  <- rep(as.integer(NA), n_rows)
  data$HTF  <- rep(as.integer(NA), n_rows)
  data$ATF  <- rep(as.integer(NA), n_rows)
  data$HTGS <- rep(as.integer(NA), n_rows)
  data$HTGC <- rep(as.integer(NA), n_rows)
  data$ATGS <- rep(as.integer(NA), n_rows)
  data$ATGC <- rep(as.integer(NA), n_rows)
  data$HTGD <- rep(as.integer(NA), n_rows)
  data$ATGD <- rep(as.integer(NA), n_rows)

  data$HTP[1]  <- 0
  data$ATP[1]  <- 0
  data$HTF[1]  <- 0
  data$ATF[1]  <- 0
  data$HTGS[1] <- 0
  data$HTGC[1] <- 0
  data$ATGS[1] <- 0
  data$ATGC[1] <- 0
  data$HTGD[1] <- 0
  data$ATGD[1] <- 0

  for (i in 2:n_rows) {

    str_home_team <- data$HomeTeam[i]
    str_away_team <- data$AwayTeam[i]

    playing_teams_match_i <- c(str_home_team, str_away_team)

    data_subset <- data %>%
      dplyr::slice(1:(i - 1)) %>% ## Select all available information before match i related to today's home and away team
      dplyr::filter(HomeTeam %in% playing_teams_match_i | AwayTeam %in% playing_teams_match_i)

    data_summary <- data_subset %>%
      dplyr::mutate(
        HTP  = calculate_points(FTR, HomeTeam, AwayTeam, str_team = str_home_team),
        ATP  = calculate_points(FTR, HomeTeam, AwayTeam, str_team = str_away_team),
        HTGS = calculate_goals_scored(FTHG, FTAG, HomeTeam, AwayTeam, str_team = str_home_team),
        HTGC = calculate_goals_conceded(FTHG, FTAG, HomeTeam, AwayTeam, str_team = str_home_team),
        ATGS = calculate_goals_scored(FTHG, FTAG, HomeTeam, AwayTeam, str_team = str_away_team),
        ATGC = calculate_goals_conceded(FTHG, FTAG, HomeTeam, AwayTeam, str_team = str_away_team)
      ) %>%
      dplyr::select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, HTP, ATP, HTGS, ATGS, HTGC, ATGC) %>%
      dplyr::summarise(
        HTP = sum(HTP),
        ATP = sum(ATP),
        HTGS = sum(HTGS),
        HTGC = sum(HTGC),
        ATGS = sum(ATGS),
        ATGC = sum(ATGC),
        HTGD = HTGS - HTGC,
        ATGD = ATGS - ATGC
      )

    data$HTP[i]  <- data_summary$HTP[1]
    data$ATP[i]  <- data_summary$ATP[1]
    data$HTGS[i] <- data_summary$HTGS[1]
    data$HTGC[i] <- data_summary$HTGC[1]
    data$ATGS[i] <- data_summary$ATGS[1]
    data$ATGC[i] <- data_summary$ATGC[1]
    data$HTGD[i] <- data_summary$HTGD[1]
    data$ATGD[i] <- data_summary$ATGD[1]

    data$HTF[i] <- calculate_form(data = data_subset, n_games = 5, str_team = str_home_team)
    data$ATF[i] <- calculate_form(data = data_subset, n_games = 5, str_team = str_away_team)

    kb.utils::loop_counter(i, n_rows)

  }

  return(data)

}

calculate_points <- function(FTR, HomeTeam, AwayTeam, str_team) {

  dplyr::case_when(
    (FTR == "H" & HomeTeam == str_team) | (FTR == "A" & AwayTeam == str_team) ~ 3,
    (FTR == "D" & HomeTeam == str_team) | (FTR == "D" & AwayTeam == str_team) ~ 1,
    TRUE ~ 0)

}

calculate_goals_scored <- function(FTHG, FTAG, HomeTeam, AwayTeam, str_team) {

  dplyr::case_when(
    HomeTeam == str_team ~ FTHG,
    AwayTeam == str_team ~ FTAG,
    TRUE ~ 0L
  )

}

calculate_goals_conceded <- function(FTHG, FTAG, HomeTeam, AwayTeam, str_team) {

  dplyr::case_when(
    HomeTeam == str_team ~ FTAG,
    AwayTeam == str_team ~ FTHG,
    TRUE ~ 0L
  )

}

calculate_form <- function(data, n_games, str_team) {
  data %>%
    dplyr::filter(HomeTeam == str_team | AwayTeam == str_team) %>%
    tail(n_games) %>%
    dplyr::mutate(Form = calculate_points(FTR, HomeTeam, AwayTeam, str_team = str_team)) %>%
    dplyr::summarise(Form = sum(Form)) %>%
    dplyr::as_tibble() %>%
    dplyr::pull(Form)
}

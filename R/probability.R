#' Get lambda
#' @param data tibble with season data (see load_seasons)
#' @param obj glm object
#' @return input tibble is returned with additional two columns: lambda_home_team and lambda_away_team
get_lambda <- function(data, obj) {

  lambda <- predict(object = obj, type = "response") %>%
    suppressWarnings()

  n <- nrow(data) * 2
  home_id <- seq(from = 1, to = n, by = 2)
  away_id <- seq(from = 2, to = n, by = 2)

  data$lambda_home_team <- lambda[home_id]
  data$lambda_away_team <- lambda[away_id]

  return(data)

}

#' Get probability for home, draw and away win
#' @param obj glm object (use glm with join_att_and_def_team_flags)
#' @param lambda_home_team numeric value specifying lambda (= expected goals pr. game) for the home team
#' @param lambda_away_team numeric value specifying lambda (= expected goals pr. game) for the away team
#'
get_prob <- function(lambda_home_team, lambda_away_team) {

  mat <- outer(
    X = dpois(x = 0:15, lambda = lambda_home_team),
    Y = dpois(x = 0:15, lambda = lambda_away_team),
    FUN = "*"
  )

  prob_draw <- diag(mat) %>% sum()

  prob_home <- (lower.tri(mat) * mat) %>% sum() ## row/cell [2, 1] = HomeTeam 2 goals, AwayTeam 1 goal

  prob_away <- (upper.tri(mat) * mat) %>% sum()

  df_prob <- dplyr::tibble(
    prob_home, prob_draw, prob_away
  )

  return(df_prob)

}

#' Vectorized probabilities
#' @param data tibble with season data (see load_seasons)
#' @param obj glm obj
#' @param verbose logical indicating if loop counter should be on (TRUE) or off (FALSE)
#' @return tibble with input data and 6 additional columns (probabilities and ratios for home, draw and away win)
vectorized_prob <- function(data, obj, verbose) {

  n <- nrow(data)

  df_list <- list()

  for (i in 1:n) {

    df_list[[i]] <- get_prob(
      lambda_home_team = data$lambda_home_team[i],
      lambda_away_team = data$lambda_home_team[i]
    )

    if (verbose) {
      kb.utils::loop_counter(i = i, n = n)
    }

  }

  df_prob <- do.call("rbind", df_list)

  df_out <- dplyr::bind_cols(
    data,
    df_prob
  )

  return(df_out)

}

add_prob_odds_ratio <- function(data) {

  data %>%
    dplyr::mutate(
      ratio_home = prob_home * B365H,
      ratio_draw = prob_draw * B365D,
      ratio_away = prob_away * B365A
    )

}

#' Get model bets
#' @param data tibble with season data
#' @param obj glm object
#' @param verbose logical indicating if loop counter should be on (TRUE) or off (FALSE)
#' @return tibble with season data and additional column related to probability, odds, ratio and bet
get_model_bets <- function(data, obj, verbose = FALSE) {

  data_lambda <- get_lambda(data = data, obj = obj)

  data_bet <- vectorized_prob(data = data_lambda, obj = obj, verbose = verbose) %>%
    add_prob_odds_ratio()

  data_bet %>%
    bet_model()

}

#' Bet model
#' @param data tibble with bets
#' @return tibble
#'
bet_model <- function(data) {

  data %>%
    dplyr::mutate(
      bet = dplyr::case_when(
        ratio_home > 1 & ratio_home > ratio_away & ratio_home > ratio_draw ~ "H",
        ratio_away > 1 & ratio_away > ratio_home & ratio_away > ratio_draw ~ "A",
        ratio_draw > 1 & ratio_draw > ratio_away & ratio_draw > ratio_home ~ "D",
        TRUE ~ "No bet"),
      odds = dplyr::case_when(bet == "H" ~ B365H,
                              bet == "A" ~ B365A,
                              bet == "D" ~ B365D),
      correct = ifelse(bet == FTR, 1,
                       ifelse(bet != FTR & bet != "No bet", 0, NA)),
      stake = ifelse(bet != "No bet", 10, 0),
      earning = ifelse(!is.na(correct), odds * correct * stake - stake, NA)
    )

}

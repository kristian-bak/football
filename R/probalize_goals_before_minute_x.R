#' Probalize goals before minute x
#' @param list_goals list obtained from `vectorized_minutes_goals_scored`
#' @param x integer specifying minute of interest
#' @return named list with:
#' * n_games: number of games used
#' * n_games_with_goals_before_minute_x
#' * p: probability of the event
#' * o: the odds of the event
#'
probalize_goals_before_minute_x <- function(list_goals, x) {

  n_games_with_goals_before_minute_x <- sapply(X = list_goals, FUN = any_goals_before_minute_x, x = x) %>%
    sum(na.rm = TRUE)

  n_games <- length(list_goals)

  p <- n_games_with_goals_before_minute_x / n_games

  o <- implied_odds(p = p)

  p_opposite <- 1 - p
  o_opposite <- implied_odds(p = p_opposite)

  out <- list("x" = x,
              "n_games" = n_games,
              "n_games_with_goals_before_minute_x" = n_games_with_goals_before_minute_x,
              "p" = p,
              "o" = o,
              "p_opposite" = p_opposite,
              "o_opposite" = o_opposite)

  return(out)

}

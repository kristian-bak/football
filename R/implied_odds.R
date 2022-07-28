#' Implied odds
#' @param p probabilities (numeric values)
#' @return numeric vector of same dimension as p
#'
implied_odds <- function(p) {
  1 / p
}

#' Implied probablity
#' @param o odds (numeric value)
implied_probability <- function(o, ignore_margen = FALSE, margen = 1.05, n_outcomes = 2) {

  if (ignore_margen) {
    eps <- 0
  } else {
    eps <- 4.5 * ((margen - 1) / n_outcomes)
  }

  1 / (o + eps)
}



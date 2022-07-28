#' Any goals before minute x
#' @param goals numeric vector with goalscoring minutes
#' @param x integer specifying the minute of interest
#' @return logical, TRUE if any goals where scored before the given minute
#'
any_goals_before_minute_x <- function(goals, x) {

  (goals < x) %>%
    any()

}

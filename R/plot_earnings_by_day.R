#' Plot earnings by day
#' @param data tibble with bets (see bet_model)
#' @return plotly object
plot_earnings_by_day <- function(data) {

  data %>%
    dplyr::group_by(Date) %>%
    calculate_earnings() %>%
    dplyr::mutate(balance = cumsum(earnings)) %>%
    plotly::plot_ly(x = ~Date, y = ~balance, type = "scatter", mode = "lines")

}

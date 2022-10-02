#' Plot earnings by day and model
#' @param data tibble with bets
#' @param title character string with plot title
#' @return plotly object
plot_earnings_by_day_and_model <- function(data, title = "") {

  data %>%
    calculate_earnings_by_strategy(f = list(bet_home, bet_away, bet_draw,
                                            bet_favorite, bet_underdog, bet_random,
                                            bet_model),
                                   group_by_date = TRUE) %>%
    dplyr::mutate(Model = rep(c("Home", "Away", "Draw", "Fav", "Underdog", "Random", "Model"),
                              each = data %>% dplyr::pull(Date) %>% unique() %>% length())) %>%
    dplyr::group_by(Model) %>%
    dplyr::mutate(balance = cumsum(earnings),
                  total_bets = cumsum(n_bets),
                  total_correct = cumsum(n_correct),
                  total_accuracy = round(100 * (total_correct / total_bets), 2),
                  total_stake = cumsum(stake),
                  return = round(100 * (balance / total_stake), 2),
                  text = paste0("Return: ", return, "<br>",
                                "Stake: ", total_stake, "<br>",
                                "Accuracy: ", total_accuracy)) %>%
    plotly::plot_ly(x = ~Date, y = ~balance, color = ~Model, type = "scatter", mode = "lines",
                    text = ~text) %>%
    plotly::layout(title = title)


}

#' Calculate earnings
#' @param data tibble with bets (see bet_model)
#' @return tibble with earning
#'
calculate_earnings <- function(data) {

  data %>%
    dplyr::summarise(stake = sum(stake),
                     n_bets = sum(bet != "No bet"),
                     n_correct = sum(correct, na.rm = TRUE),
                     accuracy = 100 * (n_correct / n_bets),
                     earnings = sum(earning, na.rm = TRUE),
                     return = 100 * (earnings / stake),
                     median_odds = median(odds, na.rm = TRUE))

}

#' Calculate earnings by strategy
#' @param data tibble with bets
#' @param f list of functions to evaluate (options: bet_home, bet_away, bet_draw and bet_model)
#' @param group_by_date logical, indicating if calculations should be made grouped by date (default is FALSE)
#' @return tibble with earnings
calculate_earnings_by_strategy <- function(data, f, group_by_date = FALSE) {

  n <- length(f)
  data_list <- list()

  for (i in 1:n) {

    data_list[[i]] <- data %>%
      {if (group_by_date) dplyr::group_by(.data = ., Date) else {.}} %>%
      f[[i]]() %>%
      calculate_earnings()

  }

  do.call("rbind", data_list)


}

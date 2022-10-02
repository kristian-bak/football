#' Bet home
#' @param data tibble with season data
#' @return tibble with additional columns
#'
bet_home <- function(data) {

  data %>%
    dplyr::mutate(
      bet = "H",
      odds = B365H,
      correct = ifelse(bet == FTR, 1, 0),
      stake = 10,
      earning = odds * correct * stake - stake
    )

}

#' Bet away
#' @param data tibble with season data
#' @return tibble with additional columns
#'
bet_away <- function(data) {

  data %>%
    dplyr::mutate(
      bet = "A",
      odds = B365A,
      correct = ifelse(bet == FTR, 1, 0),
      stake = 10,
      earning = odds * correct * stake - stake
    )

}

#' Bet draw
#' @param data tibble with season data
#' @return tibble with additional columns
#'
bet_draw <- function(data) {

  data %>%
    dplyr::mutate(
      bet = "D",
      odds = B365D,
      correct = ifelse(bet == FTR, 1, 0),
      stake = 10,
      earning = odds * correct * stake - stake
    )

}

#' Bet underdog
#' @param data tibble with season data
#' @return tibble with additional columns
#'
bet_underdog <- function(data) {

  data %>%
    dplyr::mutate(
      bet = dplyr::case_when(
        B365H > B365A & B365H > B365D ~ "H",
        B365A > B365H & B365A > B365D ~ "A",
        B365D > B365H & B365D > B365A ~ "D",
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

#' Bet favorite
#' @param data tibble with season data
#' @return tibble with additional columns
#'
bet_favorite <- function(data) {

  data %>%
    dplyr::mutate(
      bet = dplyr::case_when(
        B365H < B365A & B365H < B365D ~ "H",
        B365A < B365H & B365A < B365D ~ "A",
        B365D < B365H & B365D < B365A ~ "D",
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

#' Bet random
#' @param data tibble with season data
#' @param c integer specifying the seed (default is 1)
#' @return tibble with additional columns
#'
bet_random <- function(data, c = 1) {

  set.seed(c)

  data %>%
    dplyr::mutate(
      bet = sample(x = c("H", "A", "D"), size = 1),
      odds = dplyr::case_when(bet == "H" ~ B365H,
                              bet == "A" ~ B365A,
                              bet == "D" ~ B365D),
      correct = ifelse(bet == FTR, 1, 0),
      stake = 10,
      earning = odds * correct * stake - stake
    )

}

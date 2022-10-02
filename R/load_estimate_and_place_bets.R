#' Load estimate and place bets
#' @param leauge character string with league name (default is "Premier League")
#' @param from integer with from year (2 digits, 18 meaning 2018)
#' @param to integer with to year (2 digits, 19 meaning 2019)
#' @return
load_estimate_and_place_bets <- function(league = "Premier League", from = from, to = to) {

  data <- load_seasons(league = league, from = from, to = to)

  data_mod <- get_modeling_data_simple(data = data)

  data_flags <- data_mod %>%
    join_att_and_def_team_flags()

  seasons <- data_flags %>%
    dplyr::pull(Season) %>%
    unique()

  train_seasons <- seasons %>%
    magrittr::extract(. != max(.))

  test_seasons <- seasons %>%
    magrittr::extract(. == max(.))

  train_data <- data_flags %>%
    dplyr::filter(Season %in% train_seasons)

  test_data <- data_flags %>%
    dplyr::filter(Season %in% test_seasons)

  m <- estimate_team_model(data = train_data)

  data_train_bet <- get_model_bets(data = train_data, obj = m)

  data_test_bet <- get_model_bets(data = test_data, obj = m)

  out <- list("train_seasons"  = train_seasons,
              "test_seasons"   = test_seasons,
              "data_train_bet" = data_train_bet,
              "data_test_bet"  = data_test_bet,
              "model"          = m)

  return(out)

}

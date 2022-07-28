#' Convert million char to numeric
#' @param x character vector
#'
convert_million_char_to_numeric <- function(x) {

  z <- gsub("m", "", x) %>% as.numeric()

  return(z)

}

#' Get average market value by club
#' @param season season as integer (for instance 2020. 2020 corresponds to season 2021)
#'
get_avg_market_value <- function(season) {

  url <- glue::glue(
    "https://www.transfermarkt.com/premier-league/startseite/wettbewerb/GB1/plus/?saison_id={season}"
  )

  df_transfers <- rvest::read_html(url) %>%
    rvest::html_table() %>%
    purrr::pluck(4)

  df_transfers %>%
    dplyr::select(-1) %>%
    dplyr::select(club, Squad, Foreigners) %>%
    dplyr::slice(2:dplyr::n()) %>%
    dplyr::rename(Team = club, avg_age = Squad, avg_market_value = `Foreigners`) %>%
    dplyr::mutate(avg_market_value = gsub("€", "", avg_market_value) %>% convert_million_char_to_numeric())

}


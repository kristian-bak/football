#' Get fixture
#'
get_fixture <- function() {

  url <- "https://www.worldfootball.net/teams/liverpool-fc/2023/3/"

  df_fixtures <- rvest::read_html(url) %>%
    rvest::html_table() %>%
    purrr::pluck(2)

  id <- df_fixtures %>%
    dplyr::pull(X2) %>%
    grepl("Premier League", .) %>%
    which()

  df_tmp <- df_fixtures %>%
    dplyr::slice((id + 2):nrow(df_fixtures)) %>%
    dplyr::mutate(dd = substring(X2, 1, 2),
                  mm = substring(X2, 4, 5),
                  yy = substring(X2, 7, 10),
                  Date = paste(yy, mm, dd, sep = "-") %>% as.Date()) %>%
    dplyr::rename(Game = X1, Time = X3, Home = X4, Opponent = X6) %>%
    dplyr::select(-dd, -mm, -yy, -X2)

}

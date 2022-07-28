#' Map number to club name
#' @param year year as integer (20)
#' @param league_name league name ("Premier League")
#' @param start integer indicating first page to load
#' @param finish integer indicating last page to load
#'
map_number_to_club_name <- function(year, league_name, start, finish) {

  league <- map_league_name_to_number(league_name = league_name)

  club_name <- rep(NA, finish - start)

  for (i in start:finish) {

    url <- glue::glue(
      "https://www.futbin.com/{year}/players?page=1&club={i}&league={league}"
    )

    club_name[i - start + 1] <- rvest::read_html(url) %>%
      rvest::html_nodes(".filter_row_a") %>%
      rvest::html_text() %>%
      purrr::pluck(1) %>%
      trimws()

    Sys.sleep(2)

    if ((i %% 5) == 0) {
      Sys.sleep(5)
    }

    cat("\r", i - start + 1, "of", finish - start + 1)
    flush.console()

  }

  return(club_name)

}



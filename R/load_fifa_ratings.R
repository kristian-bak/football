#' Load fifa ratings
#' @param year year as integer (for instance 20)
#' @param league_name league name (for instance "Premier League")
#' @param club_name club name (for instance "Liverpool". See `get_team_names`
#' @param nax_pages maximal number of pages to load
#' @export
#'
load_fifa_ratings <- function(
  year = 20,
  league_name = "Premier League",
  club_name = "Liverpool",
  max_pages = 10) {

  data_list <- list()
  page_number <- 1
  go_search <- TRUE

  while (page_number <= max_pages & go_search) {

    url <- get_futbin_url(
      year = year,
      league_name = league_name,
      club_name = club_name,
      page_number = page_number
    )

    webpage <- kb.utils::catch_error(rvest::read_html(url))

    if (is.null(webpage$error)) {

      webpage <- webpage$value

    } else {

      stop("For page ", i, " the following error message was given: ", webpage$error)

    }

    tables <- webpage %>%
      rvest::html_table()

    tab <- tables[[3]][, c("X1", "X2", "X3", "X4")]

    if (tab[1, "X1"] == "No Results") {

      go_search <- FALSE

    } else {

      data_list[[page_number]] <- tab

    }

    cat(" -", page_number)

    page_number <- page_number + 1

  }

  df_out <- do.call("rbind", data_list) %>%
    dplyr::rename(
      Name = X1,
      Rating = X2,
      Position = X3,
      Version = X4
    )

  return(df_out)

}

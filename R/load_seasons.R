#' Load one season
#' @param season character string indicating season, for instance "2122"
#'
load_one_season <- function(season) {

  url <- paste0("https://www.football-data.co.uk/mmz4281/", season, "/E0.csv")

  read.csv(url, header = TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(Season = season) %>%
    dplyr::select(Season, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, FTHG, FTAG, HTR, HTHG, HTAG, Referee,
                  HS, AS, HST, AST, HF, AF, HC, AC, HY, AY, HR, AR) #%>%
  #dplyr::filter((HomeTeam == team1 & AwayTeam == team2) | (HomeTeam == team2 & AwayTeam == team1))

}

#' Season sequence
#' @param from from year (integer), for instance 18
#' @param to to year (integer), for instance 22
#'
season_seq <- function(from, to) {

  if (!is.numeric(from) | !is.numeric(to)) {
    stop("from and to must be integer")
  }

  years1 <- seq(from = from, to = to - 1)
  years2 <- seq(from = from + 1, to = to)

  paste0(years1, years2)

}

#' Load seasons
#' @inheritParams season_seq
#' @param verbose logical indicating if looper counter should be on (TRUE) or off (FALSE)
#' @return tibble with football results
#'
load_seasons <- function(from, to, verbose = TRUE) {

  seasons <- season_seq(from = from, to = to)

  n <- length(seasons)

  data_list <- list()

  for (i in 1:n) {

    data_list[[i]] <- load_one_season(
      season = seasons[i]
    )

    if (verbose) {

      cat("\r", i, "of", n)
      flush.console()

    }

  }

  df_out <- do.call("rbind", data_list)

  return(df_out)

}

#' Load one season
#' Get URL for loading data
#' @param league character string with league name (Premier League or La Liga)
#' @param season character string indicating season, for instance "2122"
#'
get_url <- function(league, season) {

  prefix <- "mmz4281"

  if (league == "Premier League") {
    suffix <- "E0"
  } else if (league == "La Liga") {
    suffix <- "SP1"
  } else {
    stop("Only Premier League and La Liga supported at the moment")
  }

  url <- glue::glue("https://www.football-data.co.uk/{prefix}/{season}/{suffix}.csv") %>%
    as.character()

  return(url)

}

#' Load one season
#' @param season character string indicating season, for instance "2122"
#'
load_one_season <- function(league = "Premier League", season) {

  url <- get_url(league = league, season = season)

  read.csv(url, header = TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(Season = season,
                  dd = substring(Date, 1, 2),
                  mm = substring(Date, 4, 5),
                  yy = substring(Date, 7, 10),
                  Date = paste(yy, mm, dd, sep = "-") %>% as.Date()) %>%
    dplyr::select(Season, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR, FTHG, FTAG, HTR, HTHG, HTAG,
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
#' @param league character string with league name (Premier League or La Liga)
#' @param verbose logical indicating if looper counter should be on (TRUE) or off (FALSE)
#' @return tibble with football results
#'
load_seasons <- function(league = "Premier League", from, to, verbose = TRUE) {

  seasons <- season_seq(from = from, to = to)

  n <- length(seasons)

  data_list <- list()

  for (i in 1:n) {

    data_list[[i]] <- load_one_season(
      league = league,
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

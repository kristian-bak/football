#' Validate season
#' @param season character string with season (for instance "2021-2022")
#'
validate_season <- function(season) {

  if (!is.character(season)) {
    stop("season must be a character")
  }

  if (nchar(season) != 9) {
    stop("season must consists of 9 characters")
  }

  y1 <- substring(season, 1, 4) %>% as.integer()
  y2 <- substring(season, 6, 10) %>% as.integer()

  if (y1 >= y2) {
    stop("season must be a character on the form year1-year2 where year2 > year1")
  }

  return(invisible(TRUE))



}

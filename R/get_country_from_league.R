#' Get country from league
#' @param league league name (character string, for instance "premier-league" or "primera-division")
get_country_from_league <- function(league) {

  if (league == "premier-league") {
    country <- "eng"
  } else if (league == "primera-division") {
    country <- "esp"
  } else {
    stop("Only premier-league and primera-division are supported so far")
  }

  return(country)

}

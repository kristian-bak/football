#' Map league to number
#' @param league_name character string with leage name (for instance "Premier League")
#' @return integer
#'
map_league_name_to_number <- function(league_name) {

  if (league_name == "Premier League") {

    return(13)

  } else {

    stop("For now only Premier League is supported")

  }

}

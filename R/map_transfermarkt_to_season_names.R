#' Map transfermarkt names to season names
#'
map_transfermarkt_to_season_names <- function(names_transfermarkt, names_season) {

  z <- gsub(" FC", "", names_transfermarkt) %>%
    gsub(" & Hove Albion", "", .)

  z[z == "Leeds United"]            <- "Leeds"
  z[z == "Leicester City"]          <- "Leicester"
  z[z == "Manchester City"]         <- "Man City"
  z[z == "Manchester United"]       <- "Man United"
  z[z == "Tottenham Hotspur"]       <- "Tottenham"
  z[z == "Newcastle United"]        <- "Newcastle"
  z[z == "West Bromwich Albion"]    <- "West Brom"
  z[z == "West Ham United"]         <- "West Ham"
  z[z == "Wolverhampton Wanderers"] <- "Wolves"

  if (!identical(sort(z), sort(names_season))) {
    warning("map_transfermarkt_to_season_names did not go well")
  }

  return(z)

}

#' Map fifa team name to uk data
#'
map_fifa_team_name_to_uk_data <- function(x) {

  x[x == "Brighton & Hove Albion"]  <- "Brighton"
  x[x == "Leeds United"]            <- "Leeds"
  x[x == "Leicester City"]          <- "Leicester"
  x[x == "Manchester City"]         <- "Man City"
  x[x == "Manchester United"]       <- "Man United"
  x[x == "Newcastle United"]        <- "Newcastle"
  x[x == "Norwich City"]            <- "Norwich"
  x[x == "Tottenham Hotspur"]       <- "Tottenham"
  x[x == "West Ham United"]         <- "West Ham"
  x[x == "Wolverhampton Wanderers"] <- "Wolves"

  return(x)

}

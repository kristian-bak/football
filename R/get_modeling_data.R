#' Get modeling data
#' @param data tibble obtained from `join_fifa_ratings` using data from `load_seasons`
#'
get_modeling_data <- function(data) {

  Goals <- c(data$FTHG, data$FTAG)
  Team <- c(data$HomeTeam, data$AwayTeam)
  Home <- c(rep("1", nrow(data)),
            rep("0", nrow(data)))
  HomeAwayOVA <- c(data$HomeOVA, data$AwayOVA)
  ATT <- c(data$HomeATT, data$AwayATT)
  MID <- c(data$HomeMID, data$AwayMID)
  DEF <- c(data$HomeDEF, data$AwayDEF)

  dplyr::tibble(Goals, Team, Home, OVA, ATT, MID, DEF)

}

#' Get modeling data
#' @param data tibble obtained from `load_seasons`
#'
get_modeling_data_simple <- function(data) {

  Goals <- c(data$FTHG, data$FTAG)
  Team <- c(data$HomeTeam, data$AwayTeam)
  Opponent <- c(data$AwayTeam, data$HomeTeam)
  Home <- c(rep("1", nrow(data)),
            rep("0", nrow(data)))
  Shots <- c(data$HS, data$AS)
  ShotsOnTarget <- c(data$HST, data$AST)
  Fouls <- c(data$HF, data$AF)
  Corners <- c(data$HC, data$AC)
  YellowCard <- c(data$HY, data$AY)
  RedCard <- c(data$HR, data$AR)

  dplyr::tibble(Goals, Team, Opponent, Home, Shots, ShotsOnTarget,
                Fouls, Corners, YellowCard, RedCard)

}

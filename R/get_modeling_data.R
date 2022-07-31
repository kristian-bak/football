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

  Date <- rep(data$Date, each = 2)
  Goals <- alternate_vectors(a = data$FTHG, b = data$FTAG)
  Team <- alternate_vectors(a = data$HomeTeam, b = data$AwayTeam)
  Opponent <- alternate_vectors(a = data$AwayTeam, b = data$HomeTeam)
  Home <- rep(c("1", "0"), nrow(data)) %>% as.factor()
  Shots <- alternate_vectors(a = data$HS, b = data$AS)
  ShotsOnTarget <- alternate_vectors(a = data$HST, b = data$AST)
  Fouls <- alternate_vectors(a = data$HF, b = data$AF)
  Corners <- alternate_vectors(a = data$HC, b = data$AC)
  YellowCard <- alternate_vectors(a = data$HY, b = data$AY)
  RedCard <- alternate_vectors(a = data$HR, b = data$AR)
  B365H <- rep(data$B365H, each = 2)
  B365A <- rep(data$B365A, each = 2)
  B365D <- rep(data$B365D, each = 2)

  dplyr::tibble(Date, Goals, Team, Opponent, Home, Shots, ShotsOnTarget,
                Fouls, Corners, YellowCard, RedCard, B365H, B365A, B365D)

}

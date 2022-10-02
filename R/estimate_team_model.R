#' Estimate team model
#' @param data tibble with attack and defence flags (see join_att_and_def_team_flags)
estimate_team_model <- function(data) {

  form <- get_team_formula(data, intercept = TRUE, home = TRUE)

  m <- glm(form, data = data, family = poisson())

  return(m)

}

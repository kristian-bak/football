#' Get team formula
#' @param data tibble with modeling data and attack and defence flags (see join_att_and_def_team_flags)
#' @param home logical indicating if Home variable should be included in the formula
#' @return formula with all attack and defence terms for a design matrix with full rang
#'
get_team_formula <- function(data, intercept = FALSE, home = FALSE) {

  if (intercept) {
    init_form <- "Goals ~ 1 + "
  } else {
    init_form <- "Goals ~ -1 + "
  }

  if (home) {
    init_form <- paste0(init_form, " Home + ")
  }

  paste0(init_form, get_att_and_def_terms(predictors = names(data), home = intercept)) %>%
    as.formula()

}

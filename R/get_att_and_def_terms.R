#' Get formula
#' @param predictors character vector with predictor names
#' @param home logical indicating if Home variable should be included in attack and defence terms (if so, both attack and defence flags for Arsenal are dropped)
#' @details `get_att_and_def_terms` filters among the predictors such that only Att and Def predictors are used
#' @return formula Goals ~ -1 + pred_1 + pred_2 + ... + pred_n
#'
get_att_and_def_terms <- function(predictors, home) {

  id_att <- grep("Att", predictors)
  id_def <- grep("Def", predictors)
  #id_att <- id_att[-1]
  #
  #if (home) {
  #  id_def <- id_def[-1]
  #}

  att_def_terms <- paste(
    paste0(predictors[id_att], collapse = " + "),
    paste0(predictors[id_def], collapse = " + "),
    sep = " + "
  )

  #form <- paste0("Goals ~ -1 + ", att_def_terms) %>%
  #  as.formula()

  return(att_def_terms)

}

#' Get formula
#' @param predictors character vector with predictor names
#' @details `get_att_and_def_terms` filters among the predictors such that only Att and Def predictors are used
#' @return formula Goals ~ -1 + pred_1 + pred_2 + ... + pred_n
#'
get_att_and_def_terms <- function(predictors) {

  id <- grep("Att|Def", predictors)
  id <- id[-1]
  att_def_terms <- paste0(predictors[id], collapse = " + ")

  #form <- paste0("Goals ~ -1 + ", att_def_terms) %>%
  #  as.formula()

  return(att_def_terms)

}

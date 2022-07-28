#' Get all predictors in a dataset
#' @param data tibble (see get_modeling_data)
#' @return character vector with possible predictor names
#'
all_predictors <- function(data) {

  sapply(data, class) %>%
    magrittr::extract(. %in% c("integer", "factor", "numeric")) %>%
    names() %>%
    magrittr::extract(. != "Goals")

}

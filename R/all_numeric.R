#' All numeric
#' @param data tibble or data.frame
#' @return character vector with all numeric columns
#'
all_numeric <- function(data) {

  sapply(data, is.numeric) %>%
    which() %>%
    names()

}

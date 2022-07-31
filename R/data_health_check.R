q25 <- function(x) {
  quantile(x, prob = 0.25)
}

q75 <- function(x) {
  quantile(x, prob = 0.75)
}

count_na <- function(x) {
  sum(is.na(x), na.rm = TRUE)
}

count_zero <- function(x) {
  sum(x == 0, na.rm = TRUE)
}

count_one <- function(x) {
  sum(x == 1, na.rm = TRUE)
}

f_distr <- list(min, q25, mean, median, q75, max, sum, count_na, count_zero, count_one)

#' Data health check
#' @param data tibble, typically modeling data (see get_modeling_data_*)
#' @param target optional, character vector with target name
#'
data_health_check <- function(data, target) {

  predictors <- all_predictors(data = data)

  num <- all_numeric(data = data)

  num_predictors <- num %>%
    magrittr::extract(. %in% predictors)

  if (!missing(target)) {
    num_predictors <- c(num_predictors, target)
  }

  data_summary_overall <- data %>%
    dplyr::summarise(dplyr::across(.cols = num_predictors, .fns = f_distr)) %>%
    rename_distr_cols()

  data_summary_home_away <- data_mod %>%
    dplyr::group_by(Home) %>%
    dplyr::summarise(dplyr::across(.cols = num_predictors, .fns = f_distr)) %>%
    rename_distr_cols()

  data_summary_team <- data_mod %>%
    dplyr::group_by(Team) %>%
    dplyr::summarise(dplyr::across(.cols = num_predictors, .fns = f_distr)) %>%
    rename_distr_cols()

  data_summary_year <- data_mod %>%
    dplyr::mutate(Year = substring(Date, 1, 4)) %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(dplyr::across(.cols = num_predictors, .fns = f_distr)) %>%
    rename_distr_cols()

  out <- list(
    "data_summary_overall" = data_summary_overall,
    "data_summary_home_away" = data_summary_home_away,
    "data_summary_team" = data_summary_team,
    "data_summary_year" = data_summary_year
  )

  return(out)

}

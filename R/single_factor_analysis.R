#' Single factor analysis
#' @param data tibble with modeling data (see get_modeling_data_*)
#' @param predictors character vector with predictor names
#' @param verbose logical indicating if loop counter should be on (default is TRUE)
#'
single_factor_analysis <- function(data, predictors, verbose = TRUE) {

  p <- length(predictors)

  adj_r_squared <- str_p_values <- rep(NA, p)

  for (i in 1:p) {

    ## For single factor it is not meaningful to remove the intercept as variables such as Home cannot be removed even tough they are insignificant if the intercept is included
    form <- paste0("Goals ~ + ", predictors[i])

    m <- glm(formula = form, family = poisson(), data = data)

    s <- summary(m)

    p_values <- s$coefficients[, 4] %>%
      magrittr::extract(names(.) != "(Intercept)")

    if (length(p_values) > 1) {

      str_p_values[i] <- paste0(names(p_values), ": ", round(p_values, 5), collapse = ", ")

    } else {

      str_p_values[i] <- round(p_values, 5)

    }

    adj_r_squared[i] <- kb.utils::adj_r_squared_for_glm(obj = m)

    if (verbose) {
      kb.utils::loop_counter(i = i, n = p)
    }

  }

  dplyr::tibble(predictors, adj_r_squared, str_p_values) %>%
    dplyr::arrange(dplyr::desc(adj_r_squared))

}

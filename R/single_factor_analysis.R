#' Single factor analysis
#' @param data tibble with modeling data (see get_modeling_data_*)
#' @param predictors character vector with predictor names
#' @param target character string with target/response variable (default is "Goals")
#' @param verbose logical indicating if loop counter should be on (default is FALSE)
#'
single_factor_analysis <- function(data, predictors, target = "Goals", verbose = FALSE) {

  p <- length(predictors)

  adj_r_squared <- str_p_values <- r_squared_with_mae <- rep(NA, p)

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
    r_squared_with_mae[i] <- r_squared_using_mae(obj = m, target = target)

    if (verbose) {
      kb.utils::loop_counter(i = i, n = p)
    }

  }

  dplyr::tibble(predictors, adj_r_squared, r_squared_with_mae, str_p_values) %>%
    dplyr::arrange(dplyr::desc(adj_r_squared))

}

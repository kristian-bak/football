#' Stepwise esimation
#' @param data tibble with attack and defence flags (see join_att_and_def_team_flags)
#' @param alpha signifiance level (default is 0.05)
#' @param max_iteration maximum number of steps
#' @param intercept intercept, logical indicating an intercept will be estimated
#' @examples
#' if (FALSE) {
#' models <- stepwise_estimation(data = data_flags)
#' }
stepwise_estimation <- function(data, alpha = 0.05, max_iteration = 40, intercept = TRUE) {

  go_step <- TRUE

  data_step <- data

  i <- 1

  model_list <- list()

  while(i <= max_iteration & go_step) {

    form <- get_team_formula(data = data_step, intercept = intercept)

    m <- glm(formula = form, family = poisson(), data = data_step)

    model_list[[i]] <- m

    s <- summary(m)

    p_values <- s$coefficients[, 4] %>%
      magrittr::extract(names(.) != "(Intercept)")

    id <- which.max(p_values)

    insignf_col[i] <- names(p_values)[id]
    insignf_val[i] <- p_values[id]

    if (insignf_val[i] > alpha) {

      data_step <- data_step %>%
        dplyr::select(-insignf_col[i])

    } else {

      insignf_col[i] <- NA
      insignf_val[i] <- NA
      go_step <- FALSE

    }

    i <- i + 1

    kb.utils::loop_counter(i = i, n = max_iteration)

  }

  out <- list(
    "iterations" = i - 1,
    "df_insignf" = dplyr::tibble(insignf_col, insignf_val),
    "models" = model_list
  )

  return(out)

}

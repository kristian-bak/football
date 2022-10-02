#' Evaluate performance
#' @param test_data test data to evaluate performance based on
#' @param obj glm object
#' @param target character string with response name
#'
evaluate_performance <- function(test_data, obj, target) {

  adj_r_squared_loglik <- kb.utils::adj_r_squared_for_glm(obj = model)

  adj_r_squared_mae_train <- r_squared_using_mae(obj = model)

  adj_r_squared_mae_train <- r_squared_using_mae(obj = model, data = test_data, target = target)

  mae_train <- mae(x = obj$fitted.values, y = obj$data[[target]])

  p_test <- suppressWarnings(predict(object = obj, newdata = test_data, type = "response"))

  y_test <- test_data[[target]]

  mae_test  <- mae(x = p_test, y = y_test)

  df_out <- dplyr::tibble(
    adj_r_squared_loglik, adj_r_squared_mae_train, mae_train, mae_test
  )

  return(df_out)

}

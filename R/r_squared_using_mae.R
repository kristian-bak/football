#' Mean absolute error
#' @param x numeric values, typically the predictions
#' @param y numeric values, typiicaly the response variable
#'
mae <- function(x, y) {

  (y - x) %>%
    abs() %>%
    mean()

}

#' R squared calculated using mean absolute error (MAE) rather than likelihood function
#' @param obj glm object
#' @param data optional, data to evaluate r squared on (default is training data taken from the glm object)
#' @param target character string specifying the target/response variable
#'
r_squared_using_mae <- function(obj, data, target = "Goals") {

  if (!missing(data)) {

    p <- suppressWarnings(predict(object = obj, newdata = data, type = "response"))
    y <- data[[target]]

  } else {

    p <- obj$fitted.values
    y <- obj$data[[target]]

  }

  y_hat <- mean(y)

  mae_m <- mae(x = p, y = y)

  mae_0 <- mae(x = y_hat, y = y)

  1 - (mae_m / mae_0)

}

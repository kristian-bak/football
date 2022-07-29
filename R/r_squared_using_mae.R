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
#' @param target character string specifying the target/response variable
#'
r_squared_using_mae <- function(obj, target = "Goals") {

  mae_m <- mae(x = obj$fitted.values, y = obj$data[[target]])

  mae_0 <- mae(x = mean(obj$data[[target]]), y = obj$data[[target]])

  1 - (mae_m / mae_0)

}

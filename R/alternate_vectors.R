#' Alternate vectors
#' @param a vector of any class
#' @param b vector of any class
#' @details Vector a and b must be of same length
#' @return vector of length 2 * a
#' @examples
#' alternate_vectors(a = 1:3, b = 11:13)
alternate_vectors <- function(a, b) {

  c(rbind(a, b))

}

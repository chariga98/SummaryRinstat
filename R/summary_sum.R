#'  Sum
#'
#' @description  This function calculates the sum of a numeric vector, either a simple sum or a weighted sum.
#'
#' @param x A numeric vector.
#' @param weights Optional numeric vector of weights for weighted sum calculations (default is NULL).
#' @param na.rm Logical. Should NA values be removed from the input vector `x` before calculation (default is FALSE)?
#' @param na_type A character string specifying how to handle NAs if `na.rm` is TRUE (default is "").
#' @param ... Additional arguments (not used in this function).
#'
#' @return The sum value of the input numeric vector `x`.
#'
#' @export
#'
#' @examples
#' # Example 1: Calculate the sum of a numeric vector
#' data <- c(1, 2, 3, 4, 5)
#' summary_sum(data)
#'
#' # Example 2: Calculate the sum with NA removal
#' data <- c(1, 2, NA, 4, 5)
#' summary_sum(data, na.rm = TRUE)
#'
#' # Example 3: Calculate the weighted sum
#' data <- c(1, 2, 3, 4, 5)
#' weights <- c(0.1, 0.2, 0.3, 0.2, 0.2)
#' summary_sum(data, weights = weights)

summary_sum <- function (x, weights = NULL, na.rm = FALSE, na_type = "", ...) {
  if(na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else {
    if (missing(weights) || is.null(weights)) return(sum(x, na.rm = na.rm))
    else return(sum(x * weights, na.rm = na.rm))
  }
}
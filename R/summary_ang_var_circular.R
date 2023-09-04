#'  Angular Variance of Circular Data
#'
#' This function calculates the angular variance of circular data.
#'
#' @param x A vector of circular data, in radians.
#' @param na.rm A logical value indicating whether to remove NA values from the input vector before calculation.
#' @param na_type A character string specifying how NA values should be treated. Options include "omit", "na.fail", or "propagate".
#' @param ... Additional arguments to be passed to the circular::angular.variance function.
#'
#' @return The angular variance of the input circular data.
#'
#' @export
#'
#' @examples
#' x <- circular::circular(c(0.1, 1.2, 2.5, NA, 3.8))
#' summary_ang_var_circular(x) # Calculates angular variance without removing NAs
#' summary_ang_var_circular(x, na.rm = TRUE) # Calculates angular variance after removing NAs
#' #summary_ang_var_circular(x, na.rm = TRUE, na_type = "omit") # Calculates angular variance after removing NAs using "omit" method

summary_ang_var_circular <- function (x, na.rm = FALSE, na_type = "", ...) {
  if (length(x) == 0 || (na.rm && length(x[!is.na(x)]) == 0)) return(NA)
  if (na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else return(circular::angular.variance(x, na.rm = na.rm))
}

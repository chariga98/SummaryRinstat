#'  Circular Range 
#'
#' This function calculates the circular range of circular data and optionally performs a test for uniformity.
#'
#' @param x A vector of circular data in radians.
#' @param test A logical value indicating whether to perform a test for uniformity (default is FALSE).
#' @param na.rm A logical value indicating whether to remove NA values from the input vector before calculation (default is FALSE).
#' @param finite A logical value indicating whether to consider finite values only (default is FALSE).
#' @param control.circular A list of control parameters to customize the circular range calculation.
#' @param na_type A character string specifying how NA values should be treated. Options include "omit", "na.fail", or "propagate" (default is an empty string).
#' @param ... Additional arguments to be passed to the circular::range.circular function.
#'
#' @return The circular range of the input circular data, and optionally the result of the uniformity test.
#'
#' @export
#'
#' @examples
#' x <- circular::circular(c(0.1, 1.2, 2.5, 3.8, 5.0))
#' summary_range_circular(x) # Calculates the circular range
#' summary_range_circular(x, test = TRUE) # Calculates the circular range and performs a uniformity test
#' #summary_range_circular(x, na.rm = TRUE, na_type = "omit") # Calculates the circular range after removing NAs using "omit" method

summary_range_circular <- function (x, test = FALSE, na.rm = FALSE, finite = FALSE, control.circular = list(), na_type = "", ...) {
  if (length(x) == 0 || (na.rm && length(x[!is.na(x)]) == 0)) return(NA)
  if (na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else return(circular::range.circular(x, test = test, na.rm = na.rm, finite = finite, control.circular = control.circular)[[1]])
}

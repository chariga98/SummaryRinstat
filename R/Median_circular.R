#'  Median of Circular Data
#'
#' This function calculates the median of a circular data vector {x}, taking into account circular nature of the data.
#'
#' @param x A vector containing circular data for which the median needs to be calculated.
#' @param na.rm A logical value indicating whether missing values ({NA}s) should be removed before calculation.
#' @param na_type A character string specifying the type of missing values check to perform. Options include "n", "prop", "n_non_miss", "FUN", and "con".
#' @param ... Additional arguments to be passed to the circular::median.circular function.
#'
#' @return The circular median of the input data vector {x}. If {na.rm} is {TRUE}, missing values are removed before calculation. If the data is empty or all missing ({NA}) values, the result will be {NA}.
#'
#' @export
#'
#' @examples
#' # Example 1: Calculate circular median without removing missing values
#' data <- circular::circular(c(0, pi/2, pi, 3*pi/2))
#' summary_median_circular(data)
#'
#' # Example 2: Calculate circular median after removing missing values
#' data <- circular::circular(c(0, pi/2, NA, 3*pi/2))
#' summary_median_circular(data, na.rm = TRUE)
#'
#
summary_median_circular <- function (x, na.rm = FALSE, na_type = "", ...) {
  if (!na.rm & anyNA(x)) return(NA)
  if (na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else return(circular::median.circular(x, na.rm = na.rm)[[1]])
}

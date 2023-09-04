#' #' Calculate Minimum of Circular Data
#'
#' This function calculates the minimum of a circular data vector {x}.
#'
#' @param x A vector containing circular data for which the minimum needs to be calculated.
#' @param na.rm A logical value indicating whether missing values ({NA}s) should be removed before calculation.
#' @param names A logical value indicating whether the result should be named.
#' @param type An integer indicating the type of quantile computation. Options include 1 to 9.
#' @param na_type A character string specifying the type of missing values check to perform. Options include "n", "prop", "n_non_miss", "FUN", and "con".
#' @param ... Additional arguments to be passed to the circular::quantile.circular function.
#'
#' @return The minimum of the input circular data vector {x}. If {na.rm} is {TRUE}, missing values are removed before calculation. If the data is empty or all missing ({NA}) values, the result will be {NA}.
#'
#' @export
#'
#' @examples
#' # Example 1: Calculate minimum without removing missing values
#' data <- circular::circular(c(0, pi/2, pi, 3*pi/2))
#' summary_min_circular(data)
#'
#' # Example 2: Calculate minimum after removing missing values
#' data <- circular::circular(c(0, pi/2, NA, 3*pi/2))
#' summary_min_circular(data, na.rm = TRUE)
#'
#'
summary_min_circular <- function (x, na.rm = FALSE, names = FALSE, type = 7, na_type = "", ...) {
  if (length(x) == 0 || (na.rm && length(x[!is.na(x)]) == 0) || (!na.rm & anyNA(x))) return(NA)
  if (na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else return(circular::quantile.circular(x, probs = 0, na.rm = na.rm, names = names, type = type)[[1]])
}

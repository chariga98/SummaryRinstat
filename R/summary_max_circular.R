#' Calculate Maximum of Circular Data
#'
#' This function calculates the maximum of a circular data vector \code{x}.
#'
#' @param x A vector containing circular data for which the maximum needs to be calculated.
#' @param na.rm A logical value indicating whether missing values (\code{NA}s) should be removed before calculation.
#' @param names A logical value indicating whether the result should be named.
#' @param type An integer indicating the type of quantile computation. Options include 1 to 9.
#' @param na_type A character string specifying the type of missing values check to perform. Options include "n", "prop", "n_non_miss", "FUN", and "con".
#' @param ... Additional arguments to be passed to the circular::quantile.circular function.
#'
#' @return The maximum of the input circular data vector \code{x}. If \code{na.rm} is \code{TRUE}, missing values are removed before calculation. If the data is empty or all missing (\code{NA}) values, the result will be \code{NA}.
#'
#' @export
#'
#' @examples
#' # Example 1: Calculate maximum without removing missing values
#' data <- circular::circular(c(0, pi/2, pi, 3*pi/2))
#' summary_max_circular(data)
#'
#' # Example 2: Calculate maximum after removing missing values
#' data <- circular::circular(c(0, pi/2, NA, 3*pi/2))
#' summary_max_circular(data, na.rm = TRUE)
#'
summary_max_circular <- function (x, na.rm = FALSE, names = FALSE, type = 7, na_type = "", ...) {
  if (length(x) == 0 || (na.rm && length(x[!is.na(x)]) == 0) || (!na.rm & anyNA(x))) return(NA)
  if (na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else return(circular::quantile.circular(x, probs = 1, na.rm = na.rm, names = names, type = type)[[1]])
}

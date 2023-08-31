#' Mean of Circular Data
#'
#' This function calculates the mean of circular data provided in vector {x}.
#'
#' @param x A vector containing circular data for which the mean needs to be calculated.
#' @param na.rm A logical value indicating whether to remove NA values before calculating the mean.
#' @param control.circular A list of control parameters to be passed to the circular package's \code{mean.circular} function.
#' @param na_type A character vector specifying the type of missing values check to perform before calculation. Available options are "n", "prop", "n_non_miss", "FUN", and "con". If missing values check fails, \code{NA} is returned.
#' @param ... Additional arguments to be passed to the circular package's {mean.circular} function.
#'
#' @return The calculated mean of the circular data in vector {x}. If {na_type} is specified and the missing values check fails, \code{NA} is returned.
#' 
#' @export
#'
#' @examples
#' # Example 1: Calculate the mean of circular data
#' circular_data <- circular::circular(c(0, pi/2, pi, 3*pi/2, 2*pi))
#' summary_mean_circular(circular_data)
#'
#' # Example 2: Calculate the mean of circular data with NA values removed
#' circular_data_with_na <- circular::circular(c(0, NA, pi/2, NA, pi, 3*pi/2, 2*pi))
#' summary_mean_circular(circular_data_with_na, na.rm = TRUE)
#'
#
summary_mean_circular <- function(x, na.rm = FALSE, control.circular = list(), na_type = "", ...) {
  if (na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) 
    return(NA)
  else 
    return(circular::mean.circular(x, na.rm = na.rm, trim = trim, control.circular = control.circular)[[1]])
}

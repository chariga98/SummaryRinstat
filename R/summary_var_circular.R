#'  Circular Variance
#'
#' This function calculates the circular variance of a circular data vector.
#'
#' @param x A vector of circular data.
#' @param na.rm Logical indicating whether to remove missing values (default is FALSE).
#' @param na_type Character specifying the type of missing values to consider if na.rm is TRUE.
#' @param ... Additional arguments to be passed to circular::var.circular() function.
#'
#' @return The circular variance of the input data.
#'
#' @export
#'
#' @examples
#' # Example usage:
#' data <- circular::circular(c(0, pi/2, pi, 3*pi/2, 2*pi))
#' summary_var_circular(data)
#' # [1] 0.8
#'
#' #summary_var_circular(data, na.rm = TRUE, na_type = "na.fail")
#' # [1] NA
#'
#' #summary_var_circular(data, na.rm = TRUE, na_type = "na.exclude")
#' # [1] 0.8
#'

summary_var_circular <- function (x, na.rm = FALSE, na_type = "", ...) {
  if(length(x) == 0 || (na.rm && length(x[!is.na(x)]) == 0)) return(NA)
  if(na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else return(circular::var.circular(x, na.rm = na.rm))
}

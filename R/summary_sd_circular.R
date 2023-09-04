#' Summary sd circular
#'
#' This function calculates the circular standard deviation of a circular data vector.
#'
#' @param x A vector of circular data.
#' @param na.rm Logical indicating whether to remove missing values (default is FALSE).
#' @param na_type Character specifying the type of missing values to consider if na.rm is TRUE.
#' @param ... Additional arguments to be passed to circular::sd.circular() function.
#'
#' @return The circular standard deviation of the input data.
#'
#' @export
#'
#' @examples
#' # Example usage:
#' data <- circular::circular(c(0, pi/2, pi, 3*pi/2, 2*pi))
#' summary_sd_circular(data)

summary_sd_circular <- function (x, na.rm = FALSE, na_type = "", ...) {
  if(na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else return(circular::sd.circular(x, na.rm = na.rm))
}


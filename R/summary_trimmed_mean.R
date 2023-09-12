#' Trimmed Mean
#'
#' @description  This function calculates the trimmed mean of a numeric vector.
#'
#' @param x A numeric vector.
#' @param add_cols Deprecated parameter; no longer in use.
#' @param weights Optional numeric vector of weights for weighted mean calculations (default is NULL).
#' @param na.rm Logical. Should NA values be removed from the input vector `x` before calculation (default is FALSE)?
#' @param trimmed Proportion (0 to 0.5) of data to be trimmed from both ends before calculating the mean (default is 0).
#' @param na_type A character string specifying how to handle NAs if `na.rm` is TRUE (default is "").
#' @param ... Additional arguments (not used in this function).
#'
#' @return The trimmed mean value of the input numeric vector `x`.
#'
#' @export
#'
#' @examples
#' # Example 1: Calculate the trimmed mean of a numeric vector
#' data <- c(1, 2, 3, 4, 5)
#' summary_trimmed_mean(data)
#'
#' # Example 2: Calculate the trimmed mean with NA removal
#' data <- c(1, 2, NA, 4, 5)
#' summary_trimmed_mean(data, na.rm = TRUE)
#'
#' # Example 3: Calculate the trimmed mean with custom trimming
#' data <- c(1, 2, 3, 4, 5)
#' summary_trimmed_mean(data, trimmed = 0.2)

summary_trimmed_mean <- function (x, add_cols, weights = NULL, na.rm = FALSE, trimmed = 0, na_type = "", ...) {
  if( length(x)==0 || (na.rm && length(x[!is.na(x)])==0) ) return(NA)
  else {
    if(na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
    else 
      return(mean(x, na.rm = na.rm, trim = trimmed))
  }
}
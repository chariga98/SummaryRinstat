#' Summary  Mean
#'
#' This function calculates the mean of a numeric vector, either a simple mean or a weighted mean.
#'
#' @param x A numeric vector.
#' @param add_cols Deprecated parameter; no longer in use.
#' @param weights Optional numeric vector of weights for weighted mean calculations (default is NULL).
#' @param na.rm Logical. Should NA values be removed from the input vector `x` before calculation (default is FALSE)?
#' @param trim Proportion (0 to 0.5) of data to be trimmed from both ends before calculating the mean (default is 0).
#' @param na_type A character string specifying how to handle NAs if `na.rm` is TRUE (default is "").
#' @param ... Additional arguments (not used in this function).
#'
#' @return The mean value of the input numeric vector `x`.
#'
#' @export
#'
#' @examples
#' # Example 1: Calculate the mean of a numeric vector
#' data <- c(1, 2, 3, 4, 5)
#' summary_mean(data)
#'
#' # Example 2: Calculate the mean with NA removal
#' data <- c(1, 2, NA, 4, 5)
#' summary_mean(data, na.rm = TRUE)
#'
#' # Example 3: Calculate the weighted mean
#' data <- c(1, 2, 3, 4, 5)
#' weights <- c(0.1, 0.2, 0.3, 0.2, 0.2)
#' summary_mean(data, weights = weights)
#'
#' # Example 4: Calculate the trimmed mean
#' data <- c(1, 2, 3, 4, 5)
#' summary_mean(data, trim = 0.2)

summary_mean <- function (x, add_cols, weights = NULL, na.rm = FALSE, trim = 0, na_type = "", ...) {
  if( length(x)==0 || (na.rm && length(x[!is.na(x)])==0) ) return(NA)
  else {
    if(na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
    else {
      if (missing(weights) || is.null(weights))
        return(mean(x, na.rm = na.rm, trim = trim))
      else 
        return(stats::weighted.mean(x, w = weights, na.rm = na.rm))
    }
  }
}

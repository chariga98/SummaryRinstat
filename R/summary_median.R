#' Summary Median Function
#'
#' @description  This function calculates the median of a numeric vector 'x'.
#'
#' @param x A numeric vector for which you want to calculate the median.
#' @param na.rm Logical, indicating whether missing values should be removed (default is FALSE).
#' @param weights A vector of weights to be used in the calculation (default is NULL).
#' @param na_type Character string specifying how to handle missing values. Possible values include "" (default), "na.omit", "na.exclude", "na.fail".
#' @param ... Additional arguments (not used in this function).
#'
#' @return The median value of the input vector 'x'.
#'
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' summary_median(x) # Returns the median of 'x'
#' 
#' # Example with missing values and weights
#' #y <- c(1, 2, NA, 4, 5)
#' #w <- c(0.1, 0.2, 0.1, 0.3, 0.2)
#' #summary_median(y, na.rm = TRUE, weights = w, na_type = "na.omit") # Returns weighted median with missing values removed
summary_median <- function(x, na.rm = FALSE, weights = NULL, na_type = "", ...) {
  if (na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else {
    if (missing(weights) || is.null(weights)) {
      if (stringr::str_detect(class(x), pattern = "ordered") || stringr::str_detect(class(x), pattern = "Date")) {
        return(stats::quantile(x, na.rm = na.rm, probs = 0.5, type = 1)[[1]])
      } else {
        return(median(x, na.rm = na.rm))
      }
    } else {
      return(Hmisc::wtd.quantile(x, weights = weights, probs = 0.5, na.rm = na.rm))
    }
  }
}

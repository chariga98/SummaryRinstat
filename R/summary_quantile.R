#' Summary Quantile Function
#'
#' @description   This function calculates a quantile of a numeric vector 'x'.
#'
#' @param x A numeric vector for which you want to calculate a quantile.
#' @param na.rm Logical, indicating whether missing values should be removed (default is FALSE).
#' @param weights A vector of weights to be used in the calculation (default is NULL).
#' @param probs Numeric vector of probabilities with values between 0 and 1, indicating which quantiles to calculate.
#' @param na_type Character string specifying how to handle missing values. Possible values include "" (default), "na.omit", "na.exclude", "na.fail".
#' @param ... Additional arguments (not used in this function).
#'
#' @return The specified quantile(s) of the input vector 'x'.
#'
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' summary_quantile(x, probs = 0.25) # Returns the 25th percentile of 'x'
#' 
#' # Example with missing values and weights
#'  #y <- c(1, 2, NA, 4, 5)
#'  #w <- c(0.1, 0.2, 0.1, 0.3, 0.2)
#'  #summary_quantile(y, na.rm = TRUE,
#'      weights = w, probs = c(0.25, 0.75), na_type = "na.omit") # Returns weighted quantiles with missing values removed
summary_quantile <- function(x, na.rm = FALSE, weights = NULL, probs, na_type = "", ...) {
  if (!na.rm && anyNA(x)) return(NA)
  # This prevents multiple values being returned
  if (na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else {
    if (missing(weights) || is.null(weights)) {
      if (stringr::str_detect(class(x), pattern = "ordered") || stringr::str_detect(class(x), pattern = "Date")) {
        return(stats::quantile(x, na.rm = na.rm, probs = probs, type = 1))
      } else {
        return(stats::quantile(x, na.rm = na.rm, probs = probs))
      }
    }
    else {
      return(Hmisc::wtd.quantile(x, weights = weights, probs = probs, na.rm = na.rm))
    }
  }
}

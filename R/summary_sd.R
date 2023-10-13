#' Summary Standard Deviation Function
#'
#' @description  This function calculates the standard deviation of a numeric vector 'x'.
#'
#' @param x A numeric vector for which you want to calculate the standard deviation.
#' @param na.rm Logical, indicating whether missing values should be removed (default is FALSE).
#' @param weights A vector of weights to be used in the calculation (default is NULL).
#' @param na_type Character string specifying how to handle missing values. Possible values include "" (default), "na.omit", "na.exclude", "na.fail".
#' @param ... Additional arguments (not used in this function).
#'
#' @return The standard deviation of the input vector 'x'.
#'
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' summary_sd(x) # Returns the standard deviation of 'x'
#' 
#' # Example with missing values and weights
#' #y <- c(1, 2, NA, 4, 5)
#' #w <- c(0.1, 0.2, 0.1, 0.3, 0.2)
#' # Returns weighted standard deviation with missing values removed
#'# summary_sd(y, na.rm = TRUE, weights = w, na_type = "na.omit") 
summary_sd <- function(x, na.rm = FALSE, weights = NULL, na_type = "", ...) {
  if (na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else {
    if (missing(weights) || is.null(weights)) {
      return(stats::sd(x, na.rm = na.rm))
    } else {
      return(sqrt(Hmisc::wtd.var(x, weights = weights, na.rm = na.rm)))
    }
  }  
}

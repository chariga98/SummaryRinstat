#' Summary Range Function
#'
#' @description  This function calculates the range of a numeric vector 'x', which is the difference between the maximum and minimum values.
#'
#' @param x A numeric vector for which you want to calculate the range.
#' @param na.rm Logical, indicating whether missing values should be removed (default is FALSE).
#' @param na_type Character string specifying how to handle missing values. Possible values include "" (default), "na.omit", "na.exclude", "na.fail".
#' @param ... Additional arguments (not used in this function).
#'
#' @return The range (max - min) of the input vector 'x'.
#'
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' summary_range(x) # Returns the range of 'x'
#' 
#' # Example with missing values
#' #y <- c(1, 2, NA, 4, 5)
#' #summary_range(y, na.rm = TRUE, na_type = "na.omit") # Returns the range with missing values removed
summary_range <- function(x, na.rm = FALSE, na_type = "", ...) {
  if (na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else {
    return(max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
  }
}

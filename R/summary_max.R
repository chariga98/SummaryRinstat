#' Summary Maximum Value Function
#'
#' @description  This function calculates the maximum value in a numeric vector 'x'.
#'
#' @param x A numeric vector for which you want to find the maximum value.
#' @param na.rm Logical, indicating whether missing values should be removed (default is FALSE).
#' @param na_type Character string specifying how to handle missing values. Possible values include "" (default), "na.omit", "na.exclude", "na.fail".
#' @param ... Additional arguments (not used in this function).
#'
#' @return The maximum value in the input vector 'x'.
#'
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' summary_max(x) # Returns the maximum value in 'x'
#' 
#' # Example with missing values
#' #y <- c(1, 2, NA, 4, 5)
#' #summary_max(y, na.rm = TRUE, 
#'      na_type = "na.omit") # Returns the maximum value with missing values removed
summary_max <- function (x, na.rm = FALSE, na_type = "", ...) {
  # TODO: This prevents warning and -Inf from being returned. Is this desirable?
  if (length(x) == 0 || (na.rm && length(x[!is.na(x)]) == 0)) return(NA)
  if (na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else {
    return(max(x, na.rm = na.rm))
  } 
}

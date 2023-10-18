#' Summary count non missing
#'
#' @param x The input vector for which you want to count non-missing values.
#' @param ... Additional arguments (not named) that can be passed to the function.
#'
#' @return The count of non-missing (non-NA) values in the input vector `x`.
#'
#' @export
#'
#' @examples
#' summary_count_non_missing(c(1, 2, NA, 4, NA, 6))
#' # Returns: 4
summary_count_non_missing <- function(x, ...) {
  
  return(sum(!is.na(x)))
  
}

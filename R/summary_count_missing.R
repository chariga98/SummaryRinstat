#' Count Missing Values
#'
#' This function calculates the count of missing (NA) values in a vector.
#'
#' @param x A vector.
#' @param ... Additional arguments (not used in this function).
#'
#' @return The count of missing (NA) values in the input vector `x`.
#'
#' @export
#'
#' @examples
#' # Example 1: Count missing values in a numeric vector
#' data <- c(1, 2, NA, 4, 5)
#' summary_count_missing(data)
#'
#' # Example 2: Count missing values in a character vector
#' words <- c("apple", NA, "cherry")
#' summary_count_missing(words)

summary_count_missing <- function(x, ...) {
  return(sum(is.na(x)))
}
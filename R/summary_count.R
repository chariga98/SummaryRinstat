#' Count Elements
#'
#' @description  This function calculates the count (length) of elements in a vector.
#'
#' @param x A vector.
#' @param ... Additional arguments (not used in this function).
#'
#' @return The count (length) of elements in the input vector `x`.
#'
#' @export
#'
#' @examples
#' # Example 1: Count the elements in a vector
#' data <- c(1, 2, 3, 4, 5)
#' summary_count(data)
#'
#' # Example 2: Count the elements in a character vector
#' words <- c("apple", "banana", "cherry")
#' summary_count(words)

summary_count <- function(x, ...) {
  return(length(x))
}

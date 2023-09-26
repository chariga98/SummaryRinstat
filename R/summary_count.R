#' Summary Count Function
#'
#' @description  This function calculates the length of a vector or list.
#'
#' @param x A vector or list.
#' @param ... Additional arguments (not used in this function).
#'
#' @return The length of the input vector or list.
#'
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' summary_count(x) 
summary_count <- function(x, ...) {
  return(length(x))
}

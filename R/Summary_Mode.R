#' Summary mode
#'
#' This function calculates the mode (most frequently occurring value) of a numeric or categorical vector.
#'
#' @param x A numeric or categorical vector for which the mode needs to be calculated.
#' @param ... Additional arguments (not used in this function).
#'
#' @return The mode of the input vector {x}. If multiple values have the same highest frequency, the first one encountered will be returned as the mode. If the input vector is empty or has no mode,{NA} is returned.
#'
#' @export
#'
#' @examples
#' # Calculate the mode of a numeric vector
#' numeric_vector <- c(1, 2, 2, 3, 4, 4, 4, 5)
#' summary_mode(numeric_vector)
#'
#' # Calculate the mode of a categorical vector
#' categorical_vector <- factor(c("apple", "banana", "apple", "banana", "banana"))
#' summary_mode(categorical_vector)
#'
#' # Edge case with no mode
#' no_mode_vector <- c(1, 2, 3, 4, 5)
#' summary_mode(no_mode_vector)
#'
#' # Edge case with an empty vector
#' empty_vector <- numeric(0)
#' summary_mode(empty_vector)
summary_mode <- function(x, ...) {
  ux <- unique(x)
  out <- ux[which.max(tabulate(match(x, ux)))]
  if (is.factor(x)) out <- as.character(out)
  if (is.null(out)) return(NA)
  else return(out)
}

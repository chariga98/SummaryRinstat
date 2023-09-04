#' Q3 circular
#'
#' This function calculates the third quartile (75th percentile) of circular data.
#'
#' @param x A vector of circular data.
#' @param na.rm A logical value indicating whether to remove NA values from 'x' before calculation.
#' @param names A logical value indicating whether to return names for the quartile values.
#' @param type An integer specifying the type of circular data (default is 7).
#' @param na_type A character string specifying the type of NA handling (if any).
#' @param ... Additional arguments to be passed to 'circular::quantile.circular'.
#'
#' @return The third quartile (75th percentile) of the circular data.
#'
#' @export
#'
#' @examples
#' #summary_Q3_circular(circular_data)
#' #summary_Q3_circular(circular_data, na.rm = TRUE)
#' #summary_Q3_circular(circular_data, type = 3, names = TRUE)
summary_Q3_circular <- function(x, na.rm = FALSE, names = FALSE, type = 7, na_type = "", ...) {
  if (length(x) == 0 || (na.rm && length(x[!is.na(x)]) == 0) || (!na.rm & anyNA(x))) return(NA)
  if (na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else return(circular::quantile.circular(x, probs = 0.75, na.rm = na.rm, names = names, type = type)[[1]])
}

#'  Quantiles of Circular Data
#'
#' @description  This function calculates quantiles for circular data using the `circular::quantile.circular` function.
#'
#' @param x A vector of circular data.
#' @param probs A numeric vector of probabilities, ranging from 0 to 1, at which to calculate quantiles.
#' @param na.rm Logical. Should NA values be removed from the input vector `x` before calculation?
#' @param names Logical. Should the resulting quantiles be named?
#' @param type An integer indicating the type of quantile calculation to be performed (default is 7).
#' @param na_type A character string specifying how to handle NAs if `na.rm` is TRUE (default is "").
#' @param ... Additional arguments to be passed to the `circular::quantile.circular` function.
#'
#' @return A numeric vector of quantiles corresponding to the specified probabilities.
#'
#' @export
#'
#' @examples
#' # Example 1: Calculate quantiles for circular data
#' data <- circular::circular(c(0, 1, 2, 3, 4), type = "angles")
#' summary_quantile_circular(data, probs = c(0.25, 0.75))
#'
#' # Example 2: Calculate quantiles with NA removal
#' data <- circular::circular(c(0, 1, NA, 3, 4), type = "angles")
#' summary_quantile_circular(data, probs = c(0.25, 0.75), na.rm = TRUE)
#'
summary_quantile_circular <- function (x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = FALSE, type = 7, na_type = "", ...) {
  if(length(x)==0 || (na.rm && length(x[!is.na(x)])==0)||(!na.rm & anyNA(x))) return(NA)
  if(na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else return(circular::quantile.circular(x, probs = probs, na.rm = na.rm, names = names, type = type)[[1]])
}
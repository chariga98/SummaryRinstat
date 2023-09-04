#' Summary Q1 Circular
#'
#' This function calculates the first quartile (Q1) for circular data using the `circular::quantile.circular` function.
#'
#' @param x A vector of circular data.
#' @param na.rm Logical. Should NA values be removed from the input vector `x` before calculation?
#' @param names Logical. Should the resulting quantile be named?
#' @param type An integer indicating the type of quantile calculation to be performed (default is 7).
#' @param na_type A character string specifying how to handle NAs if `na.rm` is TRUE (default is "").
#' @param ... Additional arguments to be passed to the `circular::quantile.circular` function.
#'
#' @return The first quartile (Q1) for the circular data.
#'
#' @export
#'
#' @examples
#' # Example 1: Calculate Q1 for circular data
#' data <- circular::circular(c(0, 1, 2, 3, 4), type = "angles")
#' summary_Q1_circular(data)
#'
#' # Example 2: Calculate Q1 with NA removal
#' data <- circular::circular(c(0, 1, NA, 3, 4), type = "angles")
#' summary_Q1_circular(data, na.rm = TRUE)
#'
#'
summary_Q1_circular <- function (x, na.rm = FALSE, names = FALSE, type = 7, na_type = "", ...) {
  if(length(x)==0 || (na.rm && length(x[!is.na(x)])==0)||(!na.rm & anyNA(x))) return(NA)
  if(na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else return(circular::quantile.circular(x, probs = 0.25, na.rm = na.rm, names = names, type = type)[[1]])
}
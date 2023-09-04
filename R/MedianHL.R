#' Calculate High-Low Median of Circular Data
#'
#' This function calculates the high-low median of a circular data vector {x}, considering different methods and a proportion of data.
#'
#' @param x A vector containing circular data for which the high-low median needs to be calculated.
#' @param na.rm A logical value indicating whether missing values ({NA}s) should be removed before calculation.
#' @param method A character vector specifying the method to use for calculating high-low median. Available options are "HL1", "HL2", and "HL3".
#' @param prop A numeric value specifying the proportion of data to be considered for calculating high-low median (used with "HL2" and "HL3" methods).
#' @param na_type A character string specifying the type of missing values check to perform. Options include "n", "prop", "n_non_miss", "FUN", and "con".
#' @param ... Additional arguments to be passed to the circular::medianHL.circular function.
#'
#' @return The high-low median of the input circular data vector {x}. If {na.rm} is {TRUE}, missing values are removed before calculation. If the data is empty or all missing ({NA}) values, the result will be {NA}.
#'
#' @export
#'
#' @examples
#' # Example 1: Calculate high-low median with method "HL1"
#' data <- circular::circular(c(0, pi/2, pi, 3*pi/2))
#' summary_medianHL_circular(data, method = "HL1")
#'
#' # Example 2: Calculate high-low median with method "HL2"
#' data <- circular::circular(c(0, pi/2, pi, 3*pi/2))
#' summary_medianHL_circular(data, method = "HL2", prop = 0.5)
#'
#' # Example 3: Calculate high-low median with method "HL3"
#' data <- circular::circular(c(0, pi/2, pi, 3*pi/2))
#' summary_medianHL_circular(data, method = "HL3", prop = 0.25)
#'
#' # Example 4: Calculate high-low median after removing missing values
#' data <- circular::circular(c(0, pi/2, NA, 3*pi/2))
#' summary_medianHL_circular(data, na.rm = TRUE, method = "HL1")
#'
summary_medianHL_circular <- function (x, na.rm = FALSE, method = c("HL1","HL2","HL3"), prop = NULL, na_type = "", ...) {
  if (!na.rm & anyNA(x)) return(NA)
  if (na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else return(circular::medianHL.circular(x, na.rm = na.rm, method = method, prop = prop)[[1]])
}

#'  Circular Rho 
#'
#' This function calculates the circular rho (mean resultant length) of circular data.
#'
#' @param x A vector of circular data, in radians.
#' @param na.rm A logical value indicating whether to remove NA values from the input vector before calculation (default is FALSE).
#' @param na_type A character string specifying how NA values should be treated. Options include "omit", "na.fail", or "propagate" (default is an empty string).
#' @param ... Additional arguments to be passed to the circular::rho.circular function.
#'
#' @return The circular rho  of the input circular data.
#'
#' @export
#'
#' @examples
#' x <- circular::circular(c(0.1, 1.2, 2.5, 3.8, 5.0))
#' summary_rho_circular(x) # Calculates the circular rho
#' summary_rho_circular(x, na.rm = TRUE) # Calculates the circular rho after removing NAs
#' summary_rho_circular(x, na_rm = TRUE, 
#'   na_type = "omit") # Calculates the circular rho after removing NAs using "omit" method

summary_rho_circular <- function (x, na.rm = FALSE, na_type = "", ...) {
  if (length(x) == 0 || (na.rm && length(x[!is.na(x)]) == 0)) return(NA)
  if (na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else return(circular::rho.circular(x, na.rm = na.rm))
}

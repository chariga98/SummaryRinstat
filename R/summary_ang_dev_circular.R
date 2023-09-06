#' Angular Deviation
#'
#' @description  This function calculates the angular deviation of a circular data vector.
#'
#' @param x A vector of circular data.
#' @param na.rm Logical indicating whether to remove missing values (default is FALSE).
#' @param na_type Character specifying the type of missing values to consider if na.rm is TRUE.
#' @param ... Additional arguments to be passed to circular::angular.deviation() function.
#'
#' @return The angular deviation of the input data.
#'
#' @export
#'
#' @examples
#' # Example usage:
#' data <- circular::circular(c(0, pi/2, pi, 3*pi/2, 2*pi))
#' summary_ang_dev_circular(data)
#' 
#'
#'# summary_ang_dev_circular(data, na.rm = TRUE, na_type = "na.fail")
#' 
#'
#'# summary_ang_dev_circular(data, na.rm = TRUE, na_type = "na.exclude")
#'  
#'

summary_ang_dev_circular <- function (x, na.rm = FALSE, na_type = "", ...) {
  if(length(x) == 0 || (na.rm && length(x[!is.na(x)]) == 0)) return(NA)
  if(na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else return(circular::angular.deviation(x, na.rm = na.rm))
}

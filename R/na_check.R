#' na checks
#'
#' This function performs various types of missing values checks on a given vector \code{x} based on specified criteria.
#'
#' @param x A vector for which missing values checks need to be performed.
#' @param na_type A character vector specifying the types of missing values checks to perform. Available options are "n" (count of missing values), "prop" (proportion of missing values), "n_non_miss" (count of non-missing values), "FUN" (custom function), and "con" (consecutive missing values).
#' @param na_consecutive_n Maximum number of consecutive missing values allowed (used with "con" type).
#' @param na_max_n Maximum count of missing values allowed (used with "n" type).
#' @param na_max_prop Maximum proportion of missing values allowed (used with "prop" type).
#' @param na_min_n Minimum count of non-missing values required (used with "n_non_miss" type).
#' @param na_FUN A custom function to determine missing values criteria (used with "FUN" type).
#' @param ... Additional arguments to be passed to the custom function {na_FUN}.
#'
#' @return {TRUE} if all specified missing values checks pass, {FALSE} otherwise.
#' 
#' @export
#'
#' @examples
#'
#' # Example 1: Using a custom function for missing values check
#' custom_check <- function(x) { sum(is.na(x)) == 0 }
#' data <- c(1, 2, 3, 4, 5)
#' na_check(data, na_type = "FUN", na_FUN = custom_check)
#'
#' # Example 2: Checking consecutive missing values
#' data <- c(1, 2, NA, NA, 5, NA)
#' na_check(data, na_type = "con", na_consecutive_n = 2)
na_check <- function(x, na_type = c(), na_consecutive_n = NULL, na_max_n = NULL, na_max_prop = NULL, na_min_n = NULL, na_FUN = NULL, ...) {
  res <- c()
  for (i in seq_along(na_type)) {
    type <- na_type[i]
    if (type %in% c("n", "'n'")) {
      res[i] <- summary_count_missing(x) <= na_max_n
    } else if (type %in% c("prop", "'prop'")) {
      res[i] <- (summary_count_missing(x) / summary_count(x)) <= na_max_prop / 100
    } else if (type %in% c("n_non_miss", "'n_non_miss'")) {
      res[i] <- summary_count_non_missing(x) >= na_min_n
    } else if (type %in% c("FUN", "'FUN'")) {
      res[i] <- na_FUN(x, ...)
    } else if (type %in% c("con", "'con'")) {
      is_na_rle <- rle(is.na(x))
      res[i] <- max(is_na_rle$lengths[is_na_rle$values]) <= na_consecutive_n
    } else {
      stop("Invalid na_type specified for missing values check.")
    }
    if (!res[i]) {
      return(FALSE)
    }
  }
  return(all(res))
}

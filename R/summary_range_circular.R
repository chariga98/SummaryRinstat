summary_ang_var_circular <- function (x, na.rm = FALSE, na_type = "", ...) {
  if(length(x)==0 || (na.rm && length(x[!is.na(x)])==0)) return(NA)
  if(na.rm && na_type != "" && !na_check(x, na_type = na_type, ...)) return(NA)
  else return(circular::angular.variance(x, na.rm = na.rm))
}
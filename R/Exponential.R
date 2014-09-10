#' Constructor for Exponential backoff classes. 
#' 
#' Builds an Exponential BackOff object. Usage:
#'    backoff_obj <- Exponential(exponent = 2 )
#'    On N'th failure delay would be set to:
#'      1st failure  :  1^2 = 1
#'      2nd failure  :  2^2 = 4
#'      3rd failure  :  3^2 = 9
#'      4th failure  :  4^2 = 16
#' @param exponent The exponent of the equiation
#' @param max_timeout The maximum amount of time the system can backoff (in seconds)
#' @export
#' @examples 
#' backoff_obj <- Exponential(exponent = 5)

Exponential <- function(max_timeout=0, exponent=1) {
  retval <- structure(list(exponent=exponent,
                           max_timeout=max_timeout,
                           failure_count=0,
                           failure_over=0,
                           failure_time=0,
                           failure_start=0,
                           backoff_in_progress=FALSE 
  ))
  
  class(retval) <- c("backoff.exponential", "backoff.base")
  retval
}

#' Returns the new back off value.
#'
#' @export

calculate_back_off.backoff.exponential <- function(x) {
  delay <- x$failure_count ^ x$exponent
  return(delay)
}

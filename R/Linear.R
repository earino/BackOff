#' Constructor for Linar backoff classes. 
#' 
#' Builds a Linear BackOff object. Usage:
#'    backoff_obj <- Linear(slope = 5, b = 0 )
#'    On N'th failure delay would be set to:
#'      y = slope * x + b;
#'      1st failure  :  5 * count + b = 5 * 1 + 0 = 5
#'      2nd failure  :  5 * 2 + 0 = 10
#'      3rd failure  :  5 * 3 + 0 = 15
#'      4th failure  :  5 * 4 + 0 = 20
#' @param slope The slope of the line
#' @param b The y intercept (or minimal backoff)
#' @param max_timeout The maximum amount of time the system can backoff (in seconds)
#' @keyword constructor
#' @export
#' @examples 
#' backoff_obj <- Linear(slope = 5, b=2)

Linear <- function(slope=1, b=0, max_timeout=0) {
  retval <- structure(list(slope=slope, 
                            b=b,
                            max_timeout=max_timeout,
                            failure_count=0,
                            failure_over=0,
                            failure_time=0,
                            failure_start=0,
                            backoff_in_progress=FALSE 
                           ))
  
  class(retval) <- c("backoff.linear", "backoff.base")
  retval
}
#' Returns the new back off value.
#'
#' @keyword back off time
#' @export

calculate_back_off.backoff.linear <- function(x) {
  my_slope <- x$slope
  my_b     <- x$b
  
  delay <- my_slope * x$failure_count + my_b
  return(delay)
}

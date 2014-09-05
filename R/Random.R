#' Constructor for Random backoff classes. 
#' 
#' Builds an Random BackOff object. Usage:
#'    backoff_obj <- Random(min = 5, max = 100 )
#'      On N'th failure delay would be set to:
#'      1st failure  :  a random number between 5 and 100 inclusive.
#'                      (5 is a possible value)
#'      2nd failure  :  a random number between 5 and 100 inclusive.
#'      3rd failure  :  a random number between 5 and 100 inclusive.
#' @param min The minimum sleep time in seconds
#' @param max The maximum sleep time in seconds
#' @param max_timeout The maximum amount of time the system can backoff (in seconds)
#' @keyword constructor
#' @export
#' @examples 
#' backoff_obj <- Random(min = 5, max = 100)
Random <- function(min=1, max=100, max_timeout=0) {
  retval <- structure(list(min=min,
                           max=max,
                           max_timeout=max_timeout,
                           failure_count=0,
                           failure_over=0,
                           failure_time=0,
                           failure_start=0,
                           backoff_in_progress=FALSE 
  ))
  
  class(retval) <- c("backoff.random", "backoff.base")
  retval
}

#' Returns the new back off value.
#'
#' @keyword back off time
#' @export

calculate_back_off.backoff.random <- function(x) {
  delay <- as.integer(runif(1, x$min, x$max))
  return(delay)
}

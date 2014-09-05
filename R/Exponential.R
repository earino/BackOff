Exponential <- function(exponent=1, max_timeout=0) {
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

calculate_back_off.backoff.exponential <- function(x) {
  delay <- x$failure_count ^ x$exponent
  return(delay)
}

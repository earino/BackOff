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

calculate_back_off.backoff.random <- function(x) {
  delay <- as.integer(runif(1, x$min, x$max))
  return(delay)
}

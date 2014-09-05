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

calculate_back_off.backoff.linear <- function(x) {
  my_slope <- x$slope
  my_b     <- x$b
  
  delay <- my_slope * x$failure_count + my_b
  return(delay)
}

BackOff <- function(max_timeout=0) {
  retval <- structure(list(max_timeout=max_timeout,
                           failure_count=0,
                           failure_over=0,
                           failure_time=0,
                           failure_start=0,
                           backoff_in_progress=FALSE,
                           val=0))
  
  class(retval) <- c("backoff.base")
  retval
}

delay <- function(x) UseMethod("delay")
sleep <- function(x) UseMethod("sleep")
success <- function(x) UseMethod("success")
reset <- function(x) UseMethod("reset")
failure <- function(x) UseMethod("failure")
valid_number_check <- function(x) UseMethod("valid_number_check")
calculate_back_off <- function(x) UseMethod("calculate_back_off")
back_off_in_progress <- function(x) UseMethod("back_off_in_progress")
# max_timeout <- function(x) UseMethod("max_timeout")
# failure_time <- function(x) UseMethod("failure_time")
# failure_over <- function(x) UseMethod("failure_over")

delay.backoff.base <- function(x) {
  if (x$failure_time == 0) {
    return(0)
  }
  
  if (x$back_off_in_progress == FALSE) {
    return(0)
  }
  
  time <- as.numeric(Sys.time())
  time_left <- x$failure_over - time
  
  if (time_left <= 0) {
    return(0)
  }
  
  return(time_left)
}

sleep.backoff.base <- function(x) {
  my_delay <- delay(x)
  if(my_delay > 0) {
    Sys.sleep(my_delay)
  }
}

success.backoff.base <- function(x) {
  eval.parent(substitute(reset(x)))
}

reset.backoff.base <- function(x) {
  eval.parent(substitute(x$failure_count <- 0))
  eval.parent(substitute(x$failure_over <- 0))
  eval.parent(substitute(x$failure_time <- 0))
  eval.parent(substitute(x$failure_start <- 0))
  eval.parent(substitute(x$back_off_in_progress <- FALSE))
}

failure.backoff.base <- function(x) {
  eval.parent(substitute(x$back_off_in_progress <- TRUE))
  eval.parent(substitute(x$failure_count <- x$failure_count + 1))  
  
  time <- as.numeric(Sys.time())
  if (x$failure_start == 0) {
    eval.parent(substitute(x$failure_start <- time))
  }

  eval.parent(substitute(x$failure_time <- time))

  more_time <- calculate_back_off(x)
  
  if (x$max_timeout > 0 && more_time > x$max_timeout) {
    more_time <- x$max_timeout
  }

  eval.parent(substitute(x$failure_over <- time + more_time))
}

calculate_back_off.backoff.base <- function(x) {
  stop("Virtual function, unimplemented for backoff_base")  
}

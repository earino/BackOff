#' Base constructor for BackOff classes. 
#' 
#' Builds a generic BackOff object (can't be used directly.)
#' @param max_timeout The maximum amount of time the system can backoff (in seconds)
#' @keyword constructor
#' @export
#' @examples 
#' backoff_obj <- BackOff(max_timeout = 5)

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

#' Function to retrieve delay
#' 
#' Delay will return the following
#'    > 0, number of seconds until the delay is over
#'    0 delay is up.  Meaning that you should do your next attempt.
#'    
#' @param
#' @keywords delay
#' @export
#' @examples
#' delay(backoff_obj) #5

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

#' Sleep the system
#' 
#' This is a shortcut for Sys.sleep(delay(backoff_obj))
#' @param
#' @keywords sleep
#' @export
#' @examples
#' sleep(backoff_obj)

sleep.backoff.base <- function(x) {
  my_delay <- delay(x)
  if(my_delay > 0) {
    Sys.sleep(my_delay)
  }
}

#' Success will clear Proc::BackOff delay
#' 
#' @param
#' @keywords reset
#' @export
#' @examples
#' success(backoff_obj)

success.backoff.base <- function(x) {
  eval.parent(substitute(reset(x)))
}

#' A reset function
#' 
#' Simply just resets backoff object back to a state in which no "backing off" exists.
#' @param
#' @keywords reset
#' @export
#' @examples
#' reset(backoff_obj)

reset.backoff.base <- function(x) {
  eval.parent(substitute(x$failure_count <- 0))
  eval.parent(substitute(x$failure_over <- 0))
  eval.parent(substitute(x$failure_time <- 0))
  eval.parent(substitute(x$failure_start <- 0))
  eval.parent(substitute(x$back_off_in_progress <- FALSE))
}

#' Failure will indicicate to the object to increment the current BackOff time.
#'
#' The calculate_back_off function is called to get the time in seconds to wait. 
#' The time waited is time+calculated_back_off time, however it is capped by 
#' max_timeout.
#' @param
#' @keywords failing
#' @export
#' @examples
#' failure(backoff_obj)

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

#' Returns the new back off value.
#'
#' This is the key function you want to overload if you wish to create your own
#' BackOff type.

calculate_back_off.backoff.base <- function(x) {
  stop("Virtual function, unimplemented for backoff_base")  
}

BackOff is a Package for Backing Off from Resources
===================================================

This package is 100% a ripoff (er, inspiration) from the fantastic
perl module [Proc::BackOff](https://metacpan.org/pod/Proc::BackOff)

    library(BackOff)  
    
    backoff_obj <- Linear(slope=5)
    while (1) {
      if (do_attempt()) {
        # success!
        success(backoff_obj)
      }      
      else {
        # oh no, something failed, a DB was down, a 
        # web service was unreachable, who knows...

        failure(backoff_obj)
      }

      if (backoff_obj$failure_count > 100) {
        error("Something is seriously wrong.")
      }

      sleep(backoff_obj) # or Sys.sleep(delay(backoff_obj))
    }

Have you ever caused yourself a headache because you hammered
on a database server or webserver nonstop with requests? Has
your devops team ever yelled about how programmers and data 
scientists just don't understand how to interact with network
resources?!

NO MORE!

Just use BackOff! This will handle automatically backing off
retries in the face of failure. Currently implemented are 3
simple backoff strategies:

* Linear - delay = slope * failure_count + minimum_backoff
* Exponential - delay = failure_count ^ exponent
* Random - Given a min and a max, just pick some number 
 
Author
======

If there are any questions, go ahead and reach out to
earino@gmail.com


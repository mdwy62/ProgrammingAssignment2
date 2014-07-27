# I think makeVector creates 4 functions and two globals:
# functions: set, get, setmean, getmean
# globals: m, x
# note that get, getmean take no arguments
#
# usage1: w <- makeVector()    creates list w consisting of 4 functions:
#                              w$set(), w$get(), w$setmean(), w$getmean()
#                              it also initializes global "m"
#         w$set(z)             assigns vector z to global "x"
#
# usage2: w <- makeVector(z)   allows skipping the w$set(z) step above.
#
# note: if data (z) changes, w$set(z) has to be rerun before cachemean(w)
#
# after either of above:
#         cachemean(w)         will either calculate mean of global x or retrieve it from global "m"



# rm(list=ls()) # clear workspace

makeVector <- function(x = numeric()) { 
  m <- NULL                               # initialize global "m"
  
  set <- function(y) {                    # "set" function
          x <<- y                         # assigns arg to global "x"
          m <<- NULL                      # (re)initializes global "m"
  }
  
  setmean <- function(mean) m <<- mean    # "setmean" assigns arg "mean" to global "m"
  
  get <- function() x                     # "get" returns global "x"
  
  getmean <- function() m                 # "getmean" returns global "m"
  
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# cachemean makes use of getmean, get, setmean functions, but where is set used?

 cachemean <- function(x, ...) {
         m <- x$getmean()
      
         if(!is.null(m)) {
                 message("getting cached data")
                 return(m)
         }
         data <- x$get()
         m <- mean(data, ...)
         x$setmean(m)
         m
 }

z <- 1:10
print(mean(z)) # for comparison 
w <- makeVector(z)
print(cachemean(w))

z <- 20:50
print(mean(z))
w$set(z)
print(cachemean(w))

#--# 

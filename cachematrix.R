# makeCacheMatrix creates/returns 4 functions:
# functions: set, get, setminv, getminv
# globals: m, x
# note that get, getminv take no arguments
#
# usage1: w <- makeCacheMatrix()
#   creates list w consisting of 4 functions:
#   w$set(), w$get(), w$setminv(), w$getminv()
#   it also initializes global "m"
#
# then:
#         w$set(z)
#   assigns matrix z to global "x"
#
# usage2: w <- makeCacheMatrix(z)
#   allows skipping the w$set(z) step above.
#
#
# note: if data (z) changes, w$set(z) HAS
# to be rerun before cacheSolve(w)
#
# after either of above:
#         cacheSolve(w)        will either calculate inverse of global x
#                               or retrieve it from global "m"

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                               # initialize global "m"
  
  set <- function(y) {                    # "set" function
          x <<- y                         # assigns arg to global "x"
          m <<- NULL                      # (re)initializes global "m"
  }
  
  setminv <- function(minv) m <<- minv    # "setminv" assigns arg "minv" to global "m"
  
  get <- function() x                     # "get" returns global "x"
  
  getminv <- function() m                 # "getminv" returns global "m"
  
  list(set = set, get = get,              # returns list of 4 functions
       setminv = setminv,
       getminv = getminv)
  
}


# makeCacheMatrix needs to be run first. See above.
#         cacheSolve(w)        will either calculate inverse of global x
#                               or retrieve it from global "m"

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
 m <- x$getminv()
 
 if(!is.null(m)) {                  
   message("getting cached data")  
   return(m)                              # if inverse exists in m, return
 }
 data <- x$get()                          # else retrieve data in x (from set())
 m <- solve(data, ...)                    # compute inverse
 x$setminv(m)                             # assign to cache m
 m                                        # return inverse
 
}

# testing - uncomment below
# t <- 30
# k <- 5
# 
# z <- rnorm(t*k)
# dim(z) <- c(t,k)
# zz   <- t(z)%*%z
# izz1 <- solve(zz)
# print("for checking/ comparison: direct solve 1")
# print(izz1)     
# 
# w <- makeCacheMatrix(zz)
# print(cacheSolve(w))
# print(cacheSolve(w)) # verify pulls from cache 2nd time
# print(sum(izz1-cacheSolve(w))) # check equality 
# 
# z <- rnorm(t*k)      # generate new data
# dim(z) <- c(t,k)
# zz   <- t(z)%*%z
# izz2 <- solve(zz)
# print("for checking/ comparison: direct solve 2")
# print(izz2)     # for checking/ comparison
# 
# w$set(zz)
# print(cacheSolve(w))
# print(cacheSolve(w)) # verify pulls from cache 2nd time
# print(sum(izz2-cacheSolve(w))) # check equality 

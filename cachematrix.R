## To reduce the need to recompute a matrice's inverse,
## the following functions create a a special list that:
## makeCacheMatrix: stores a matrix and its inverse
## cacheSolve: selectively computes or loads the the inverse

## makeCacheMatrix can will create a list from a matrix x
## that contains functions to:
## "set" the value of x
## "get" the value of x
## "setSolve" to solve for m, the inverse of x
## "getSolve" to return m, the cached inverse of x

makeCacheMatrix <- function(x = numeric()) {
  ## initialize matrix m to NULL
  ## this will store the inverse of x
  m <- NULL
  
  ## create a function "set" that assigns
  ## value y (from outside scope) to x
  ## when setting a new value, m is forced back to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## create a function "get" that returns
  ## the value of x
  get <- function() x
  
  ## create a function "setSolve" that takes the function
  ## "solve" as an argument stores the result in m
  ## i.e. it inverts the matrix m
  setSolve <- function(solve) m <<- solve
  
  ## create a function "getSolve" that returns the value
  ## m, the inverse of x
  getSolve <- function() m
  
  ## returns a list with the results of set, get, setSolve, and getSolve
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## cacheSolve is a function that will either:
## return the cached inverse of x
## or, if it has not yet been computed, solve(x)
## and update the cache for future use

cacheSolve <- function(x, ...) {
  ## run the getSolve function within the cacheMatrix x
  ## and assign the value to m
  m <- x$getSolve()
  
  ## if the resulting matrix m is not NULL, the inverse
  ## was previously computed and stored in m
  if(!is.null(m)) {
    message("getting cached data")
    ## recover the cached data and return m
    return(m)
  }
  ## OTHERWISE, continue
  ## if the resulting matrix m, from x$getSOlve() is NULL
  ## i.e. no value was already stored in m
  
  ## load the value of matrix x and store in data
  data <- x$get()
  
  ## solve for the inverse of data (same as x) and store in m
  m <- solve(data, ...)
  
  ## update the cached the value of the inverse of x with the
  ## solved result m
  x$setSolve(m)
  
  ## return the value of m
  m
}

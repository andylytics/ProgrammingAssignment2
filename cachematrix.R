### Assignment 2, Coursera R Programming, 2014-04-25

## These two functions are intended to compute and cache the inverse of a supplied square matrix.
## Caching can improve the speed of a script or program by preventing the need to perform a
## computation redundantly

## the makeCacheMatrix function creates a list of functions that can cache the inverse of the
## matrix supplied in the parameter

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## the cacheSolve function computes the inverse of the supplied matrix. If the function will
## return inverse from cache if the computation was previously performed and the matrix is
## unchanged

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  
  # check if inverse of matrix is already in cache
  if(!is.null(m)) {
    message("getting cached data...")
    return(m)
  }
  
  # if not cached compute inverse and cache
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

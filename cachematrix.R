## Two functions to handle caching of matrix inverses, as 
## they are costly to compute on demand.

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If it's already calculated, return the
## cached value.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if( !is.null(inv) ) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(a, ...)
  x$setInverse(inv)
  inv
}
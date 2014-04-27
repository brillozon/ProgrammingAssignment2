## These functions implement a cached matrix inversion.  If the matrix
## has not changed since last being inverted, the cached value is
## returned rather than re-solving the inverse.

## makeCacheMatrix - returns a cache object for the matrix.
##                   Implemented as a closure holding the
##                   cache and matrix.

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the cached data.
  inverse <- NULL;
  
  ## Store a new matrix (and clear the cache).
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## Access the current matrix.
  get <- function() x
  
  ## Cache a new value of the inverse.
  setinverse <- function(i) inverse <<- i
  
  ## Access the cached inverse.
  getinverse <- function() inverse
  
  ## Value is a list holding the closure.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
  )
}


## Returns the inverse of a matrix - either a previously inverted and
## cached value or a newly computed (and cached) value.

cacheSolve <- function(x, ...) {
  ## First check if the calculate result is cached.
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    ## Done - use the cached result.
    message("getting cached inverse")
    return(inverse)
  }

  ## Invert the matrix and cache the result.
  result <- x$get()
  inverse <- solve(result, ...)
  x$setinverse(inverse)
  
  ## Done - return the inverted matrix.
  inverse
}

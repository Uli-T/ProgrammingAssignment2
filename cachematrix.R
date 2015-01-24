## rprog-010 Programming Assignment 2
## This programme caches the inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Defines set of functions for caching the inverse of a matrix.
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}

cacheSolve <- function(x, ...) {
  ## Calculates the inverse of matrix x, otherwise calculate it.
  m <- x$getinverse()
  # if inverse matrix is cached return it
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  # otherwise calculate it
  data <- x$get()
  m <- solve(x$get())
  x$setinverse(m)
  m
}

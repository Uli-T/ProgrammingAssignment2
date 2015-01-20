## This programme caches the inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Defines functions for caching the inverse of a matrix.
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
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(x$get())
  x$setinverse(m)
  m
}
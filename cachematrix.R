## cachmatrix.R calculates and caches the inverse of 
## a matrix. It also checks to see if the inverse has 
## already been calculated to avoid costly re-calculations.

## The function makeCacheMatrix creates a special matrix
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the 
## matrix returned by makeCacheMatrix. If the inverse
## has already been calculated and the matrix has not 
## changed, the function returns the inverse from the
## cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

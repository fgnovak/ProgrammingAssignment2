## These functions create a matrix object and with this matrix, it can calculate 
## the inverse and cache the result or retrieve a previous result from cache.  

## The function 'makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The function 'cacheSolve' cmoputes the inverse of a special matrix returned by
## 'makeCacheMatrix' or, if the inverse has already been calculated and the matrix
## has not changed, then 'cacheSolve retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' returned by 'makeCacheMatrix'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solv(data, ...)
  x$setinv(inv)
  inv
}

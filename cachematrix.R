## This file contains functions for computing and/or caching the inverse of a matrix.

##
##  Function makeCacheMatrix solves the inverse of the given matrix.
##  (The function assume that the matrix supplied is always invertible).
##
##  Function makeCacheMatrix creates a special "matrix" 
##  which is really a list containing a function to:
##
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse
##    4. get the value of the inverse
##

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # to get the value of the matrix
  get <- function() x
  # to set the inverse
  setinverse <- function(solve) inv <<- solve
  # to get the inverse
  getinverse <- function() inv
  # create a special "matrix" object containing the functions
  list(set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
}


##  Function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed),
##  then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  # check if the inverse is already cached and return.
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if not cached before, compute the inverse, cache it and return the inverse.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
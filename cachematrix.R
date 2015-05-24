## This file contains a pair of functions that cache the inverse of a matrix.
## The function include:
##
## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
##
## cacheSolve: Computes the inverse of the special "matrix" retrurned by makeCacheMatrix.  If
## the inverse is already been calculated (and the matrix has not changed), then the cacheSolve
## should retrieve the inverse from the cache.

## makeCacheMatrix:
## Creates a special "matrix" by
## setting the value of the matrix
## getting the value of the matrix
## setting the value of the inverse of the matrix
## getting the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  #initialize
  minv <- NULL
  
  #set the matrix
  setmatrix <- function (y) {
    x <<- y
    minv <<- NULL
  }
  
  #get the matrix
  getmatrix <- function () x
  
  #set the matrix inverse
  setinvmatrix <- function(solve) minv <<- solve
  
  #get the matrix inverse
  getinvmatrix <- function() minv
  
  #return the list of functions
  list(setmatrix = setmatrix,
       getmatrix = getmatrix,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}

## cacheSolve:
## Calculates the inverse of the special "matrix".
## If the inverse is already calculated, then returns the inverse from the cache.
cacheSolve <- function(x, ...) {
  #get cache value if exists
  invmatrix <- x$getinvmatrix()
  if (!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  
  #if cache value does not exist, calculate the inverse and store in cache.
  data <- x$getmatrix()
  invmatrix <- solve(data)
  x$setinvmatrix(invmatrix)
  
  #Return a matrix that is the inverse of 'x'
  invmatrix
}

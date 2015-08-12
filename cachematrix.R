## Goal: Write a pair of functions that cache the inverse of a matrix

## The follwing function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(A = matrix()) {
  I <- NULL
  
  setmatrix <- function(B) {
    A <<- B
    I <- NULL
  }
  
  getmatrix <- function() A
  
  setinverse <- function(inv) I <<- inv
  
  getinverse <- function() I
  
  list(setmatrix=setmatrix, getmatrix=getmatrix,
       setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  I <- x$getinverse()
  if (!is.null(I)) {
    message("getting cached data")
    return(I)
  } 
  mat <- x$getmatrix()
  I <- solve(mat, ...)
  x$setinverse(I)
  I
}


## Test
m <- matrix(c(-1, -2, 1, 1), 2, 2)
x <- makeCacheMatrix(m)
x$getmatrix()
inv <- cacheSolve(x)
inv
inv <- cacheSolve(x)
inv


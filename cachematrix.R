## Put comments here that give an overall description of what your
## functions do

## Constructs a vector able to hold a cached matrix, including 
## some relevant data about the matrix. The original cached
## matrix (or associated matrix) is the value of the parameter. 
##
## param m: the original matrix to be cached
## return: vector with four functions, which is originally 
##   associated with matrix m. That vector is as follows: 
##    [1] function set(nm) - setter operation that sets matrix nm
##         as the cached matrix in the vector
##    [2] function get() - getter operation that returns the
##         matrix currently cached by the vector
##    [3] function setinverse(inv) - sets the inverse of the 
##         cached matrix to matrix given by parameter inv
##    [4] function getinverse() - returns null if the inverse
##         matrix for the current associated matrix has not been set; 
##         otherwise, it returns the inverse matrix also cached in 
##         the vector
##
makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(nm) {
    if (all(dim(m) == dim(nm)))
      if (all(m == nm)) {
        ## message("Matrix is already cached.")
        return()
      }
    ## message("This is a new matrix.")
    m <<- nm
    inv <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Function to return the inverse matrix of the cached matrix in
## a vector that was previously constructed using function makeCacheMatrix. 
## If the inverse of the cached matrix is already computed, the action is
## to retrieve it from the vector and return that retrieved inverse.
## Otherwise, it computes the inverse matrix and saves it in
## the cache vector before returning it.
##
## param mv: the vector corresponding to the cached matrix. This version
##  presumes that the cached matrix is invertible; otherwise, an error 
##  message is generated when trying to compute the inverse matrix.
## param ...: other parameters that can be included for the R function
##  solve(), which is the function used here to compute the inverse 
##  matrix
## return: the inverse matrix corresponding to the one currently 
##  associated with mv
##
cacheSolve <- function(mv, ...) {
  ## Return a matrix that is the inverse of cached matrix 'mv'
  ## mv is assumed to be an invertible matrix
  
  inv <- mv$getinverse()
  if (!is.null(inv)) {
    ## message("Inverse matrix is already cached.")
    return(inv)
  }
  ## message("Inverse matrix, not cached. Computing it.")
  mat <- mv$get()
  inv <- solve(mat, ...)
  mv$setinverse(inv)
  inv
}
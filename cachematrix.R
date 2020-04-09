## Assignment: Caching the Inverse of a Matrix
## This code helps to cache the inverse of a matrix by executing few functions

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( x = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## set the value of the matrix
  set <- function( matrix ) {
    x <<- matrix
    i <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() {
    ## return the value of the matrix
    x
  }

  ## set the value of the inverse
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## get the value of the inverse
  getInverse <- function() {
    ## return the value of the inverse
    i
  }
  
  ## return a list of matrices
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not
## changed), then the "cacheSolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## return the inverse (if its already set)
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## get the matrix from our object
  data <- x$get()
  
  ## calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## set the inverse to the object
  x$setInverse(m)
  
  ## return the matrix
  m
}

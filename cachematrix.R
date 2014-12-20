## This file consists of two functions: makeCacheMatrix and cacheSolve. These
## two functions can be used together to create a matrix that is capable of
## caching a solved inverse of the given matrix. This can save time when the 
## inverse of a matrix is needed multiple times in a piece of code.

## The makeCacheMatrix function needs a matrix as argument, and attaches
## 4 functions on the given matrix, namely:
##  * set
##  * get
##  * setInverse
##  * getInverse
## These four functions be used to set or get the matrix, and set or get the 
## inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function can be used with a matrix constructed using the
## makeCacheMatrix function. This function computes the inverse of the matrix
## using the solve() function. If the inverse was already computed, it returns
## the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

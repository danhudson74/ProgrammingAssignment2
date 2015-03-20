## This R script is used to create a pair of R functions that 
## together can reduce the computations needed to perform matrix
## inversion by storing the inverse of a matrix in a cache for
## later retrieval if the inverse has already been calculated and
## the matrix has not changed since the inverse was cached.

## The makeCacheMatrix function creates a special matrix object that
## can store its calculated inverse in a cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  ## Generate a list of defined functions 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the special
## matrix object returned by the makeCacheMatrix fucntion. If the
## inverse has already been calculated and the matrix has not
## changed, cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  ## Return a matrix that is the inverse of 'x'
}
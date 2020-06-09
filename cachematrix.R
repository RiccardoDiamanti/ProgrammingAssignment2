## This first function caches the inverse of a matrix
## this is helpful anytime you would like to recall the value
## of it, without having to recompute it

## The function first sets a function on a matrix, then recalls
## and gets the function and caches the inverse as m in a different 
## environment, and finally gets the inverse that we assigned to m 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x {
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  }
}


## This second function gets the inverse of the matrix calculated in 
## makeCacheMatrix and solves it, meaning that it gets the inverse of the 
## previously calculated inverse. The function finally prints the value.

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
## The following functions caches a "matrix" object that can cache its inverse
## and also compute/retrieve the inverse

## The "makeCacheMatrix" sets the elements of the matrix, then gets the elements
## of the matrix, then sets the elements of the matrix inveresely, then gets the 
## inverted matrix elements.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list (set = set, get = get,
        setinverse = setinverse, 
        getinverse = getinverse)
}


## The purpose of this function was to retrieve the inverse of a matrix returned 
## by the "makeCacheMatrix" above. I changed mean -> inverse and m -> inv from 
## the "cachemean" example.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

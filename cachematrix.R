## The following code contains a pair of functions that cache the inverse
## of a matrix.

## The 'makeCacheMatrx' function is responsible for creating a special type
## of matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The 'cacheSolve' function is responsible for computing the inverse of the
## special matrix passed to it as an argument 'x'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  ## Check to see if the inverse matrix already exists in memory.
  ## If it does, simply return it.
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## Otherwise, compute the inverse of matrix 'x'
  data <- x$get()
  inverse <- solve(data, ...)
  ## Place the inverse matrix in memory (i.e. cahce it)
  x$setinverse(inverse)
  inverse
}

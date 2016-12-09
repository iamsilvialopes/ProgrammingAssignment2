## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ## aims to create a list containing 4 operations
  m <- NULL                                 ## creates empty vector named "m"
  set <- function(y) {                      ## 1st: set the value of the matrix
    x <<- y                                 ## assigns y to x in a different envrironment
    m <<- NULL
  }
  get <- function() x                       ## 2nd: getting the value of the matrix
  setinverse <- function(solve) m <<- solve  ## 3rd: computing the inverse of a matrix
  getinverse <- function() m               ## 4rd: getting the inverse of a matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## This function looks for the inverse of matrix x. If it exists, shows a message and
## returns the inverse. If it doesn't exist, then the inverse is calculated. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
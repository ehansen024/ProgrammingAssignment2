## The makeCacheMatrix should be used in conjunction with the cacheSolve function. Together they will return
## the inverse of the inputted matrix, either by calling the inverse if it is already computed, or by
## computing it on the spot.

## This function takes a matrix as an argument and creates a list (or "special" matrix) that holds the matrix
## itself, and its inverse once it's computed.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes a "special" matrix (like those created by the makeCacheMatrix function) as
## an argument, and looks for the inverse of the original matrix. If it has already been computed and
## stored in the special matrix, the function finds it, and returns it. Otherwise, it will compute and return
## the inverse on the spot.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m     
}

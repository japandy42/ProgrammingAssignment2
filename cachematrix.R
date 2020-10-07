## These functions work in tandem to cache an inverse matrix

## Creates a list that stores the inverse of the matrix once it has
## been run through cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Computes the inverse of the input matrix (which must have been
## passed through makeCacheMatrix) or retrieves it if it has already 
## been computed.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
        ## Return a matrix that is the inverse of 'x'
}

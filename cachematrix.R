## A pair of functions that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse (solve).
# It creates a special "matrix", which is really a list containing a function to:
# set the value of the matrix
# get the value of the matrix
# set the value of the solve
# get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached solve")
    return(s)
  }
  data <- x$get()
  # Computing the inverse of a square matrix can be done with solve(x)
  s <- solve(data, ...)
  x$setsolve(s)
  s
}


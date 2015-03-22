## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL
  set <- function(y) {
    x <<- y
    inverted <<- NULL
  }

  get <- function() x
  setinvert <- function(invert) inverted <<- invert
  getinvert <- function() inverted
  list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invertedData <- x$getinvert()
  if (!is.null(invertedData)) {
     message("returning cached result")
     return(invertedData)
  }
  data = x$get()
  invertedData = solve(data, ...)
  x$setinvert(invertedData);
  invertedData
}


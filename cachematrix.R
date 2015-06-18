## These functions allow you to cache the inverse of a matrix.
## This allows us to save some time rather than running the computation again.

## The first function creates a list containing functions to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse of a matrix
## - get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {           ## - set the value of the matrix
    x <<- y
    invM <<- NULL
  }
  get <- function() x           ## - get the value of the matrix    
  setinvM <- function(inverse) invM <<- inverse   ## - set the value of the inverse of a matrix
  getinvM <- function() invM     ## - get the value of the inverse of a matrix
  list(set = set,
       get = get,
       setinvM = setinvM,
       getinvM = getinvM)
  
}


## This function first checks if the inverse of a matrix has been computed.
## If it has, it returns that cached inverse and saves processing time.
## If it has not been created, then it calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invM <- x$getinvM()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  data <- x$get()
  invM <- solve(data, ...)
  x$setinvM(invM)
  invM
  
}
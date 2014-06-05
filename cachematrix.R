## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  ##set: used to set the matrix object. Can be used to change the one
  ##originally passed in.
  set <- function(y) {
    x <<- y
    ##Setting this null insures that we won't return the inverse of the old matrix
    cachedInverse <<- NULL 
  }
  
  ##get: used to get the matrix object associated with the cached inverse
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cachedInverse <- x$getInverse()
  if(!is.null(cachedInverse)) {
    message("getting cached data")
    return(cachedInverse)
  }
  data <- x$get()
  cachedInverse <- solve(data, ...)
  x$setInverse(cachedInverse)
  cachedInverse
}

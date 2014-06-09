## Contains two functions that are used to store the cached value of 
## the inverse of a matrix.  This could be used to save computation 
## cost of calculating the inverse of a matrix over and over
## Example:  
##  > m <- matrix(c(1,2,3, 11,12,13, 3,5,6), nrow = 3, ncol = 3)
##  > cachMatrix <- makeCacheMatrix(m)
##  > invM <- cacheSolve(cachMatrix)  
## The first time cacheSolve is run on the cachMatrix variable the 
## inverse of m is calculated and returned.
##  > invFromCach <- cacheSolve(cachMatrix)
## any subsequent time it is called the cached value of the inverse 
## is returned wiht out the need to re-run the inverse calculation

## makeCacheMatrix: is basically a wrapper around a standard matrix object
## that provides the ability to cache the invers of the wrapped matrix.
## It takes an optional signle argument that is the matrix that will have
## its inverse computed and cashed.

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


## cacheSolve:  This functions takes an object returned by the makeCacheMatrix function
## which holds a matrix and a cached value of that matrixes inverse.  If the inverse of
## the matrix has already been calculated it will returned the cached value otherwise it will
## calculate the inverse, cache it and return it.

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

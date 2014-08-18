## Below are two functions, makeCacheMatrix and cacheSolve,
## that are used to create an object that stores a matrix and 
## caches its inverse.


makeCacheMatrix <- function(x = matrix()) {
## makeCacheMatrix creates a cached matrix via the following functions:
## 1. set: set the value of the cached matrix
## 2. get: get the value of the cached matrix
## 3. setinv: set the value of its inverse
## 4. getinv: get the value of its inverse  
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) {
## cacheSolve returns a matrix that is the inverse of 'x'
## x is assumed to be invertible
  
  ##First check to see if the inverse has already been calculated
  ##If so, get the mean from the cache
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  ##If not, calculate the inverse using the solve function
  data <- x$get()
  inv <- solve(data, ...)
  
  ##Set the value of the inverse in the cache via the setinv function
  x$setinv(inv)
  
  ##Return the inverse
  inv
}

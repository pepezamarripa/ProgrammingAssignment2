## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse to NULL
  inv <- NULL 
  
  # function to set matrix value and invalidate cache
  set <- function(y) { 
    x <<- y
    inv <<- NULL
  }
  
  # function to retrieve matrix value
  get <- function() x 
  
  # setter function to set inverse
  setinv <- function(inverse) inv <<- inverse 
  
  # function to retrieve inverse
  getinv <- function() inv
  
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv() # retrieve inverse from cache
  
  if (!is.null(inv)) { # if inverse is cached, return it
    message("getting cached data")
    return(inv)
  }
  
  # retrieve matrix from object
  mat <- x$get() 
  
  # calculate inverse
  inv <- solve(mat, ...) 
  
  # cache inverse
  x$setinv(inv) 
  
  # return inverse
  inv 
}

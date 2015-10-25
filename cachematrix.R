## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## set initial value to NULL, clearing existing values
  invMatrix <- NULL

  ## get the value of the Matrix
  get <- function() x

  ## get the value of the inverse 
  getinverse <- function() invMatrix
  
  ## set the value of the  Matrix
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  ## set the value to the inverse 
  setinverse <- function(value) invMatrix <<- value
  
  ## List calls
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## get the cached value
  invMatrix <- x$getinverse()
  ## if not null use the cached one
  if(!is.null(invMatrix)) {
    message("Cached result")
    ## return the cached value
    return(invMatrix)
  }
  ## we do not a cached value
  data <- x$get()
  ## lets solve it
  invMatrix <- solve(data)
  ## set the value
  x$setinverse(invMatrix)
  ## return the new value
  invMatrix
}

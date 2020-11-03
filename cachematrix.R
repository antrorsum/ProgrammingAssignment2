## Put comments here that give an overall description of what your
## functions do

## Creates a function, `makeCachMatrix` creates a special "matrix", 
## which is really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the inverse of the matrix 
## 4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix` above. If the inverse has already been 
## calculated (and the matrix has not changed), then the `cachesolve` 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

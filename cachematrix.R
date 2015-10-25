## The two functions defined below, makeCacheMatrix() and cacheSolve(), calculate and cache the inverse of 
## inversable matrix x

## makeCacheMatrix creates a matrix object x that can cache itself and its inverse; x$get() returns matrix x;
## x$set(y) sets a new value y for matrix x and caches it (note that y needs to be a reversible matrix);
## x$getinv() returns inv, the inverse of matrix x;
## x$setinv(inver) sets a new value inver for inv, the inverse of matrix x; 
## if inv has not been calculated and cached (value is NULL), cacheSolve(x) calculates the inverse of x 
## and uses x$setinv() to replace and cache the solution;
## Note that x$set() resets the value of inv to NULL: whenever x's value is changed, its inverse needs to be 
## recalculated again by solveCache because it cannot be accessed from the cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inver) inv <<- inver
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve() returns a matrix that is the inverse of matrix x (the argument of both cacheSolve() 
## and makeCacheMatrix()). If matrix x's inverse has already been calculated and stored in the chache, 
## the value of x$getinv() as defined under makeCacheMatrix() will not be null (!is.null(inv)). 
## If that's the case, cacheSolve retrieves the inverse from the cache with return(inv).
## If however inv is NULL (it has not been calculated and cached), the inverse of x (inv) 
## gets calculated with solve(data) and stored in the cache with the function x$setinv(inv).

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv   
}

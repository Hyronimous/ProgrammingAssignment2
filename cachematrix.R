## Put comments here that give an overall description of what your
## functions do

## the makeCacheMatrix function will cache the matrix and inversion.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) m <<- inv
    getInverse <- function() m
    list(set =  set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## The cachesolve will determine if the inverse is already calculated on the 
## same matrix. If it is it will return the cache. If not it will recaculate the 
## inverse.


cacheSolve <- function(x, ...) {
     m <- x$getInverse()
      if(!is.null(m)) {message("getting cached data")
      return(m)
      }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

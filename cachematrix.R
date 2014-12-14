## if the solve of a matrix is time needs to be calculated very often, for example in a loop,
## it can be very timeconsuming.
## This file contains two functions wich can be used to optimize this by caching the value of the solve if 
## the matrix has not changed.



## functon makeCacheMatrix 
# creates a cached matrix as a list of functions 
# contsisting of the functions get and set to set or get the
# matrix and the functions getsolve and setsolve to set or get 
# cached solve of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## functon cacheSolve
# this function takes a cached matrix cunstructed with makeCacheMatrix
# and returns the cached solve if there is already a cached value.
# Otherwitse it gets the data of the matrix, calculates the solve from it
# and writes the calculated sovle to the cache.
# Finaly it returns the calculated solve.


cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
        ## Return a matrix that is the solve of 'x'
}


# example:
# > c = rbind(c(1, -1/4), c(-1/4, 1))
# > cm = makeCacheMatrix(c)
# > cacheSolve(cm)
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# > cacheSolve(cm)
# getting cached data
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# > solve(c)
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667

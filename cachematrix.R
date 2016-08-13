##Matrix inversion is usually a costly computation and 
##there may be some benefit to caching the inverse of
##a matrix rather than compute it repeatedly.
##The assignment is to write a pair of functions 
##that cache the inverse of a matrix.


##The first function, makeVector creates a special "vector", 
##which is really a list containing a function to:
##Set and get the value of the vector, and the second function
##set and get the value of the mean.
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")  
    return(inv)
                     }
  obj <- x$get()
  inv <- solve(obj, ...)
  x$setInverse(inv)
  inv
}

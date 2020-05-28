## Caching the Inverse of a Matrix

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinver <- function(inversion) m <<- inversion
  getinver <- function() m
  list(set = set, get = get,
       setinver = serinver,
       getinver = getinver)
}


## This function computes the inverse of the special "matrix" returned by 

## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinver()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinver(m)
  m
}

## makeCacheMatrix creates a list of functions based on the
## matrix argument and cacheSolve returns the inverse of the
## matrix from makeCacheMatrix.

## makeCacheMatrix creates a list of four functions to:
## 1. Set the value of the matrix ('set' function)
## 2. Get the value of the matrix ('get' function)
## 3. Set the inverse value of the matrix ('setinverse' function)
## 4. Get the inverse value of the matrix ('getinverse' function)

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## cacheSolve returns the inverse of the matrix created in
## the makeCacheMatrix function.
## If the inverse was previously computed, the function will
## retrieve the output of the previous computation via the 
## getinverse function. Otherwise it will compute and sets the 
## output via the setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}

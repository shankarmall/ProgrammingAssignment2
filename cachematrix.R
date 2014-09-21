## makeCacheMatrix generates the inverse of a matrix, stores it in cache. Retrieves 
## it if it exists for the matrix, bringing in efficiency.
## Based on prototype code structure for assignment 2.

## setinv sets up a new matrix
## getinv retrieves the inverse of a matrix. Function used with cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheinv checks whether inverse exists for the matrix, and retrieves it from  
## cache. If inverse does not exist, functions generates the inverse of the matrix

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- cor(data, ...)
  x$setinv(m)
  m
}
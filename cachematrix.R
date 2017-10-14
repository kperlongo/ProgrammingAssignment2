## The makeCacheMatrix and cacheSolve functions together invert a 
## matrix and cache the result in order to accelerate subsequent access of the inverted matrix.


## The makeCacheMatrix function establishes a parent environment in which to store the matrix
## and its inverse.

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


## The cacheSolve function calculates or retrieves a previously cached inverse of the matrix from makeCacheMatrix().

cacheSolve <- function(x, ...) {
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
## Invert a matrix
## the inverted matrix is stored cached on the matrix object 
## so the inverte will only ble calculated once.
## assumption: the matrix provided is invertable.

## This a cached matrix where then inverse of a matrix can be cached for multiple usage.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Invert a matrix and cache the result. 
## If the matrix is already solved, the cached result is returned
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()

  i <- solve(data, ...)
  x$setinverse(i)
  i
}




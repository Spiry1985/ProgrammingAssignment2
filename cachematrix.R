##makes inverse of the matrix and caches it

makeCacheMatrix <- function(X = mat()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve ##do inverse of matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## this function calculates the inverse of matrix returned by makeCache Matrix
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {             ## checks if it was not done before
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)  ## if not , inverse the matrix
  x$setinverse(m)
  m
}
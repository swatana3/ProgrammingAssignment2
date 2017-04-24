##Functions which help in dealing with costly computations in inverse matrices
##by cacheing inverse matrix values reducing the need to compute it repeatedly 

## Returns a list object, or "special" matrix, from the given invertible matrix
## that gives it the ability for the function to store inverse values or, in other words, to cache it. 

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


## Returns the inverse matrix value from cache after receiving the "special" matrix and if the one in 
## cache is unavaiable it will compute its own and return that

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

makeCacheMatrix <- function(x = matrix()) {
# set the matrix
    m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
# get the matrix  
  get <- function() x
# set the inverse  
  setinverse <- function(solve) m <<- solve
# get the inverse  
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
# get the inverse from makeCacheMatrix  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
# calculate the inverse and set it in the cache   
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


#Making the cachematrix and the inverse of the function for the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#creating the cachesolve to inverse the matrix

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
#exampple for input
a <- diag(3,7)
a
CacheMatrix <- makeCacheMatrix(a)
cacheSolve(CacheMatrix)
b <- diag(1,9)
b
CacheMatrix <- makeCacheMatrix(b)
cacheSolve(CacheMatrix)

##The 2 functions below are used to cache the inverse of a matrix

## The first function, makeCacheMatrix which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of the matrix
##get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  #holds the cached value. Initially it is ser to NULL
  m <- NULL
  
  #Used to store the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Returns the stored matrix
  get <- function() x
  
  #Caches the inverse of the matrix
  setinv <- function(solve) m <<- solve
  
  #returns the cached value i.e. inverse of the matrix
  getinv <- function() m
  
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## the cacheSolve functions is used to get the inverse of the matrix. It first checks the
## cache if the inverse has already been computed. If the value is in the cache, it gets
## the value from the cache and returns it. If the values is not present in the cache,
## it computes the inverse and stores the value in the cache and returns the inverse.

cacheSolve <- function(x, ...) {
  #gets the cached value
  m <- x$getinv()
  
  #if the values exists, return the value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #else, compute the inverse and store in cache
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  
  #return the inverse
  m
}

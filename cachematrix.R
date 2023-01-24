## they are a pair of functions which will cache the inverse of 
## a matrix, and then retrieve the inverse from the cache.

## makeCacheMatrix() takes a matrix, and makes it into a list that 
## contains the matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setin <- function(solve) i <<- solve
  getin <- function() i
  list(set = set, get = get,
       setin = setin,
       getin = getin)
}

## cacheSolve()  computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getin()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setin(i)
  i  ## Return a matrix that is the inverse of 'x'
}



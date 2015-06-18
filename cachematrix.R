# Functions are used to compute the inverse of a matrix in a time-saving way:
# if the inverse has been computed before they retrieve it from cache.
# Example of calling the functions:
# x <- matrix( c(1,0,1,3,0,3), 2, 2)
# mat <- makeCacheMatrix(x)
# makeSolve(mat)


# Function makeCacheMatrix creates a special "matrix" that can cache its inverse.
# In fact it is a list containing a function to:
# - set the values of the matrix
# - get the values of the matrix
# - set the inverse of the matrix
# - get the inverse of the matrix

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Function makeSolve computes the inverse of the "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed), 
# it retrieves the inverse from the cache.

makeSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv  
}


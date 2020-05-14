# This pair of functions cache the inverse of a matrix to avoid re-calculation.

# The function makeCacheMatrix returns a list which includes four functions to set and get the matrix and its inverse.
# The variable m is used to store the inverse matrix when it is calculated.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<- solve(x)
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# The function cacheSolve returns the inverse matrix of 'x'.
# Inverse of the matrix is stored in the variable 'm' in makeCacheMatrix if calculation was done previously.
# If not previously calculated, it calculates the inverse of the matrix, stores the result in
# variable m, and returns its value.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("fetching cached inverse matrix..")
    return(m)
  }
  data <- x$get()
  message("calculating inverse matrix for the first time..")
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


# Example to test the function
# x <- matrix(c(1,2,2,1), 2, 2)
# a <- makeCacheMatrix(x)

# First execution
# b <- cacheSolve(a)
# b

# Subsequent executions
# b <- cacheSolve(a)
# b

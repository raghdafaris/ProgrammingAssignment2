# Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse matrix
  inverse <- NULL
  
  # Function to set the matrix
  set <- function(matrix) {
    x <<- matrix
    # Invalidate the cache when the matrix is updated
    inverse <<- NULL
  }
  
  # Function to get the matrix
  get <- function() {
    x
  }
  
  # Function to get the inverse of the matrix
  getInverse <- function() {
    inverse
  }
  
  # Function to set the inverse of the matrix
  setInverse <- function(inverseMatrix) {
    inverse <<- inverseMatrix
  }
  
  # Return a list of functions
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}

# Function to compute the inverse of the special matrix object
cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse if available
  inverse <- x$getInverse()
  
  # If the inverse is not cached, compute it and cache it
  if (!is.null(inverse)) {
    message("Getting cached inverse")
    return(inverse)
  }
  
  # If the inverse is not cached, compute it and cache it
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

# Create a special matrix object
m <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))

# Compute the inverse of the matrix
cacheSolve(m)

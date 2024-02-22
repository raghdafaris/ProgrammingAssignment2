
makeCacheMatrix <- function(x = matrix()) {
  
  #calculate the size of the input matrix 
  input_rows <- nrow(x)
  input_cols <- ncol(x)
  
  # Create an initial matrix with the same dimensions as the input matrix
  results <- matrix( nrow = input_rows , ncol = input_cols)
  
  #calculating the inverse of the matix x
  inv_mat <- solve(x)
  
  # Cache the result in the matrix created ubove 
  results <- inv_mat
  
  #return the updated matrix 
  return(results)
}
results <- makeCacheMatrix()

# this function checks the cach and x  
cacheSolve <- function(results, x,...) {
  
  # Check if the result is already cached
  if (!is.na(results)) {
    print("Using cached result")
    return(results)
    
  }
  #if the inverse is not cached then calculate it using the input matrix 
  data <- x
  notcach <- solve(data, ...)
  notcach
}
  

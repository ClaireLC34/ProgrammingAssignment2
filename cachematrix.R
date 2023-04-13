## Put comments here that give an overall description of what your
## functions allow computing and caching the inverse of a matrix

## Function which creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(mat = matrix()) {
  
  # Initialize inverse as NULL
  inv_mat <- NULL
  
  # Set the value of the matrix
  set <- function(y) {
    mat <<- y
    inv_mat <<- NULL
  }
  
  # Get the value of the matrix
  get <- function() mat
  
  # Set the value of the inverse
  setinverse <- function(solve) inv_mat <<- solve
  
  # Get the value of the inverse
  getinverse <- function() inv_mat
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Compute the inverse of the special matrix returned above after verifying if 
## it already exists in the cache 

cacheSolve <- function(mat, ...) {
  
  inv_mat <- mat$getinverse()
  
  # Check if inverse is null
  if(!is.null(inv_mat)) {
    message("getting cached data")
    # Return inverse value
    return(inv_mat)
  } 
  data <- mat$get()
  inv_mat <- solve(data, ...)
  mat$setinverse(inv_mat)
  return(inv_mat)
  
}

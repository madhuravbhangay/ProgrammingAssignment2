## makeCacheMatrix function creates a "vector", which is a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y = matrix()) {
    x <<- y
    i <<- NULL
  }
  
  # Get the value of the matrix
  get <- function() x
  
  # Set function to set the value of the inverse variable i
  setInverse <- function(inverse) i <<- inverse
  
  # Function to get the value of the inverse variable i
  getInverse <- function() i
  
  # Set the return list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function chec if the inverse of the matrix is present,
## if yes, then returns the cached value
## if no, then calculate the inverse and store the value in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Try to get the cache value
  inverse <- x$getInverse()
  
  # If value is already cached then return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # Cached value is not present, calculate inverse and cache the value
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

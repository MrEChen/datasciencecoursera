## A pair of functions that cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  ## The function makeCacheMatrix creates a list that contains
  ## a matrix and its inverse.
  
  inv <- NULL  	# inverse not computed at the time the list is created
  
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ## The function cacheSolve either retrieves the inverse cached
  ## in the list created by makeCacheMatrix (if already cached)
  ## or computes the inverse.
  
  inv <- x$getinverse()
  
  if (!is.null(inv))  	# the inverse has already been calculated
  {
    message("getting cached data")
    return(inv)		      # to skip the computation below
  }
  
  data <- x$get()
  inv <- solve(data,...)	# calculate the inverse of matrix data
  x$setinverse(inv)		  # store the inverse in the cache
  
  inv                   # Return a matrix that is the inverse of 'x'
}

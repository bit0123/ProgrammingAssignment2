## The following function is introduced tosave the computating time by caching inverse matrix to the corresponding matrix

## Cache inverse matrix
## Consists of a list of four member function for geting current matrix & reseting a new matrix which deletes the cache
## Two other member function is for geting cache value and create cache

makeCacheMatrix <- function(x = matrix()) {
  
  matrix.inverse <- NULL
  set <- function(y){
    x <<- y
    matrix.inverse <<- NULL
  }
  get <- function() x
  getSolve <- function() matrix.inverse
  setSolve <- function(mi) matrix.inverse <<- mi
  
  list(get = get,set = set,getSolve = getSolve,setSolve = setSolve)
}

## Return a matrix that is the inverse of matrix 'x' if the inverse matrix is already cached
## Otherwise inversion matrix is created using solve() util function and cache it. also return that value

cacheSolve <- function(x, ...) {
  
  matrix.inverse <- x$getSolve()
  
  # Check whether cache value is NULL or not
  # if cache exists return the cache value
  
  if (! is.null( matrix.inverse ) ){
    message("getting cached data")
    return(matrix.inverse)
  }
  
  # cache is not available
  # inverse matrix, cache the value 
  data <- x$get()
  matrix.inverse <- solve(data, ...)
  x$setSolve(matrix.inverse)
  matrix.inverse
}

## Testing

# Create matrix
#m1 <- matrix(1:4,2,2)

# Create custom cache matrix
#cache <- makeCacheMatrix(m1)

# First invocation, should not hit cache
#cacheSolve(cache)
# Second invocation, get value from cache
#cacheSolve(cache)

# re-set matrix, clean cache
#m2 <- matrix(2:5,2,2)
#cache$set(m2)

# First invocation, should not hit cache
#cacheSolve(cache)
# Second invocation, get value from cache
#cacheSolve(cache)

# Cross check
#identical(solve(m2),cacheSolve(cache))
## The following function is introduced tosave the computating time by caching inverse matrix to the corresponding matrix

## Cache inverse matrix
## Consists of a list of four member function for geting current matrix & reseting a new matrix which deletes the cache
## Two other member function is for geting cache value and create cache

makeCacheMatrix <- function(x = matrix()) {
  
  sol <- NULL
  set <- function(y){
    x <<- y
    sol <<- NULL
  }
  get <- function() x
  getIM <- function() sol
  setIM <- function(im) sol <<- im
  
  list(get = get,set = set,getIM = getIM,setIM = setIM)
  
  
}

## Return a matrix that is the inverse of matrix 'x' if the inverse matrix is already cached
## Otherwise inversion matrix is created using solve() util function and cache it. also return that value

cacheSolve <- function(x, ...) {
  m <- x$getIM()
  if(! is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setIM(m)
  m
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
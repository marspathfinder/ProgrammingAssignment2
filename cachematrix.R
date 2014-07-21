## Similar to the example given on the instructions
## the two functions will calculate and cache inverses of matrix

## function makeCacheMatrix will create a special matrix which allows the following functions:
## set/get the value of the matrix, set/get the value of inverse
## these setter/getter functions are used in the next function to cache values
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(value)  inv <<- value
  getinverse <- function() inv
  list(set = set, get = get,
       getinverse = getinverse,
       setinverse = setinverse)
}

## function cacheSolve will calculate the inverse of matrix if not previously calculated
## If so, it directly gets the value from the cache
cacheSolve <- function(x, ...) {
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

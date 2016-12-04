
## Initialize objects x and x_inverse
## Set function assigns input argument to x object in parent environment andd assigns NULL value to x_inverse in parent environment
## Defines the getter and setter for matrix x
## Assigns functions to a list in parent environment so that $ can extract later

makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) x_inverse <<- inv
  getinverse <- function() x_inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Uses get to retrieve x_inverse, if not NULL the computation is skipped and x_inverse is retrieved from cache
## If x_inverse is NULL, data object is initiated and the inverse of the matrix is calculated with 'Solve' function
## Sets the value of x_inverse in the cache with the setinverse function
cacheSolve <- function(x, ...) {
  x_inverse <- x$getinverse()
  if(!is.null(x_inverse)) {
    message("getting cached data")
    return(x_inverse)
  }
  data <- x$get()
  x_inverse <- solve(data)
  x$setinverse(x_inverse)
  x_inverse
}

## Calculates the inverse of a matrix
## If it's already been calculated, returns it

## sets or gets value of matrix, sets or get its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return a matrix that is the inverse of above function
## if the matrix is unchanged and inverse has been cached
## returns it. Otherwise it uses first function to solve and return inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'      
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

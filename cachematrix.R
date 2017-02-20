## Two functions work in combination to create an object containing
## a square matrix, find its inverse and store that value for reuse

## There is no feature that ensures that the user inputs only 
## invertible matrices into makeCacheMatrix


## A function that allows user to use the set subfunction to specify
## a square matrix, the get subfunction to return that matrix, the
## getinv subfunction to get a previously cached inverse matrix and
## the setinv subfunction which is used by cacheSolve internally

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 set <- function(y) {
   x <<- y
   inv <<- NULL
 }
 get <- function() x
 setinv <- function(inverse) inv <<- inverse
 getinv <- function() inv
 list(set = set, get = get,
      setinv = setinv,
      getinv = getinv)
}

## cacheSolve is a function that will either look up a previously 
## stored inverse using the getinv subfunction contained in
## makeCacheMatrix or, alternatively, will compute the inverse of 
## a square matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


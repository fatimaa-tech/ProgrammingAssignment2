
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of that object, using the cached value
## if the inverse has already been calculated.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL

  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  get <- function() x

  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv

  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 inv <- x$getinverse()

  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
m <- makeCacheMatrix(matrix(c(2, 1, 1, 3), 2, 2))
cacheSolve(m)   # computes the inverse
cacheSolve(m)  

## makeCacheMatrix takes a square invertible matrix (not forced but assumed)
## and returns a list of four functions. It also stores the input matrix and the 
## inverse matrix, which is NULL until computed by cacheSolve.


## This creates four functions within its own environment which can be
## called at a later time, i.e. in cacheSolve. Namely, it will set and store
## the inverse created in cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {    
    x <<- y                   
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve first checks if the inverse is NULL, i.e. has not been cached.
## If it is cached, it will simply return the stored version rather than re-compute.
## Otherwise, it computes the inverse for the given matrix with the solve() function
## and sets it using setinverse() from makeCacheMatrix.

cacheSolve <- function(x) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}


# Below is code for testing above functions
f <- makeCacheMatrix(matrix(data = rnorm(n = 9), nrow = 3, ncol = 3))
f$get()
f$getinverse()  # should be NULL first time

cacheSolve(f)
cacheSolve(f)   # do you get message "getting cached data"?

f$getinverse()  # should produce the inverse now that it has been set in cacheSolve

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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

# Below is code for testing above functions, uncomment to test
# f <- makeCacheMatrix(matrix(data = rnorm(n = 9), nrow = 3, ncol = 3))
# f$get()
# f$getinverse()  # should be NULL first time
# 
# cacheSolve(f)
# cacheSolve(f)   # do you get message "getting cached data"?
# 
# f$getinverse()  # should produce the inverse now that it has been set in cacheSolve

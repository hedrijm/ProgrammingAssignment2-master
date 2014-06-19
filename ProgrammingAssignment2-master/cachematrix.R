## Put comments here that give an overall description of what your
## functions do

## this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #set the value of the matrix
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  
  #Set the value of the inverse
  setinverse <- function(solve) i <<- solve
  
  #get the value of the inverse
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## this function computes the inverse of the special "matrix" returned by the above function.
## If the inverse has already been calculated (and the matrix has not changed), then this
## Function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  #checks if inverse has already been calculated
  i <- x$getinverse()
  
  #if yes then pulls inverse from the cache
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #if no then calculates the inverse
  data <- x$get()
  i <- solve(data, ...)
  
  #sets the value of the inverse in the cache
  x$setinverse(i)
  i
}

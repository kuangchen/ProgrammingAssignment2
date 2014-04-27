## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function takes a single argument x
## Return value: a list consisting of four methods 
## set: set the value of x, also resetting cached inverse
## get: retrieve the value of x
## setinverse: set the cached inverse
## getinverse: get the cached inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv2) inv <<- inv2
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cachedSolve calculates the inverse of a matrix x
## using cached inverse

cacheSolve <- function(x, ...) {
  #print(x)
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    # if cached inverse is found
    # then simply return the cached inverse
    message("getting cached inverse")
    return(inverse)
  }
  
  # otherwise calculates the inverse using solve
  # and store the value in cached inverse
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  
  inverse   
}

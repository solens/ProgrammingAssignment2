## Solve and keep inverse matrices in memory
## 1. Function that builds the vector of functions to set and get the solved solution
## 2. Manager function to check if the inverse has already been calculated
## and manipulate the functions in the preceding vector


## 4 functions in one vector
## set() and get() relevant matrix
## setinv() and getinv() corresponding inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solved) inv <<- solved
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve relates the inverse demand to the functions inside makeCacheMatrix function
## checks if the inverse has already been calculated
## gets the data if so
## sets the inverse if not

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

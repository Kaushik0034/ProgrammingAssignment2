## The 2 functions below create a special matrix that can cache its inverse
## The first function -makeCacheMatrix returns a list of functions that do the following
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the Inverse of the matrix (This is to manually set the inverse of the matrix)
## 4. Get the Inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function returns the inverse of the matrix
## If the matrix is set with same set of values and the function is run more than once
## then the cacheSolve function will not re-calculate the inverse from second time onwards, rather it will 
## take the Inverse matrix elements from the cache created as part of the first run
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## get the matrix and calculate the inverse matrix 
## cache the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      MatInv <- NULL
      set <- function(y) {
            x <<- y
            MatInv <<- NULL
      }
      get <- function() x
      setInv <- function(ginv) MatInv <<- solve(x)  
      getInverse <- function() MatInv
      list(set = set, get = get,
           setInv = setInv,
           getInverse = getInverse)
}


## Write a short comment describing this function
## check if the inverse matrix of x is cached or not
## if the cached matrix exists, take the cached data and return it
## if the cached matrix does not exist, solve for the inverse matrix and then return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      MatInv <- x$getInverse()
      if(!is.null(MatInv)) {
            message("getting cached data")
            return(MatInv)
      }
      data <- x$get()
      MatInv <- solve(data)
      x$setInv(MatInv)
      MatInv		
}

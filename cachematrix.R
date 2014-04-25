## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## get the matrix and calculate the inverse matrix 
## cache the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      MatInv <- NULL
      set <- function(y) {
            x <<- y				## take the input matrix
            MatInv <<- NULL		## initialize the inverse matrix
      }
      get <- function() x		## take the input matrix
      setInv <- function(ginv) MatInv <<- solve(x)  ## solve for the inverse matrix and cache it
      getInverse <- function() MatInv				## get the inverse matrix
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
      MatInv <- x$getInverse()					## get the inverse matrix 
      if(!is.null(MatInv)) {					## check if the cached data exists or not
            message("getting cached data")		## cached data exists, pull the cached data directly
            return(MatInv)						## returnt the cached data
      }
      data <- x$get()							## cached data does not exist
      MatInv <- solve(data)						## solve for the inverse matrix
      x$setInv(MatInv)							## take the inverse matrix
      MatInv									## return the inverse matrix
}

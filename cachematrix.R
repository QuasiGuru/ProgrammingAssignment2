## The following fuctions return the inverse of a matrix
## for a given matrix.  As well, the inverse is stored
## in the cache

## Function MakeCacheMatrix(x)
## ---------------------------
## @param x -- A matrix
##
## This function creates a matrix that can cache
## its inverse.  This function returns a list
## containing a function to get and set the
## value of a matrix, and set and get the
## inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      
      ## Changed Code
      ## List Containing Function
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## Function cacheSolve(x)
## ---------------------------
## @param x -- A special matrix created by 
## makeCacheMatrix function
##
## This function checks to see if the inverse of
## the matrix has already been calculated.  If so,
## the function retrieves the inverse and returns it.
## Otherwise, the function calculates the inverse of
## the matrix, caches the inverse and returns it

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      
      ## Changed Code
      ## Calculate the inverse
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

## These tow functions works tougether to compute an inverse matrix and cache the resolte
## functions do

## This function creates a set of functions that get a matrix and it´s inverse. 
#Once inverse matrix is computed, the function caches the result.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set.matrix <- function(y) {
            x <<- y
            m <<- NULL
      }
      get.matrix <- function() x
      set.inverse <- function(inverse) m <<- inverse
      get.inverse <- function() m
      list(set.matrix = set.matrix, get.matrix = get.matrix,
           set.inverse = set.inverse,
           get.inverse = get.inverse)
}


## This function computes the inverse of a matrix, since it was not been calculated and stored on cache by the function above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$get.inverse()
      if(!is.null(m)) {
            message("getting cached inverse matrix")
            return(m)
      }
      data <- x$get.matrix()
      m <- solve(data)
      x$set.inverse(m)
      m
}

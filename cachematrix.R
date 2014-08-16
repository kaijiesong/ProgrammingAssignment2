## Thie pair of function cache the inverse of a matrix

## This function creates a matrix to cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i<<-inverse
      getinverse <- function() i
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the matrix from the function above. If the matrix
## has been computed, the inverse can be retrived from the cache

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cache data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}

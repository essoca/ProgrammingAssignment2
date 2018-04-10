## The following functions implement the calculation of 
## the inverse of a given square (invertible) matrix by
## storing the matrix/inverse in cache and then retrieving
## or computing the inverse according to whether or not it 
## has been calculated yet.

## This function inputs the matrix for which the inverse is
## to be calculated and creates an environment to store the 
## matrix and its inverse. Methods to set and retrieve the 
## matrix/inverse are returned in a list.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y){
        x <<- y
        i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get, setinverse = setinverse,
           getinverse = getinverse)
}


## This functions inputs the list returned above and checks
## whether the inverse has already been calculated, in which 
## case its value is retrieved from cache. Otherwise the
## computation is done with the solve() function, stored in
## cache and the value of the inverse returned.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if (!is.null(i)) {
          message("getting cached data")
          return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}

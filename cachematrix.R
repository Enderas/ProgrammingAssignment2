## These functions cache a matrix in environment with functions to manage it

## This function cache an object x in environment with 4 functions
## set and get allow to cache a matrix
## setinverse and getinverse allow to cache the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   
   # Set a matrix in cache
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   
   # Get the cached matrix
   get <- function() x
   
   # Set the inversed matrix in cache
   setinverse <- function(inverse) m <<- inverse
   
   # Get the inversed matrix from cache
   getinverse <- function() m
   
   # Return an object with the 4 functions
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function check if the inversed matrix is stored in environment.
## If it's not the case, it commpute the inversed matrix and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getinverse()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m
   
}

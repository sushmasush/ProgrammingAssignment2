#Functions to cache the inverse of a matrix. 
#Assumption made: matrix supplied is always invertible.

#makeCacheMatrix function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
	        set <- function(y) {
		                x <<- y
				cachedInverse <<- NULL
			}
			get <- function() x
			setInverse <- function(inverse) cachedInverse <<- inverse #Assign the cached object inverse
			getInverse <- function() cachedInverse
			list(set = set, get = get,
			     setInverse = setInverse,
			     getInverse = getInverse)
}
#cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
         invFunc <- x$getInverse()
         if(!is.null(invFunc)) {
                 message("getting cached data")
                 return(invFunc)
          }
          data <- x$get()
	  invFunc <- solve(data, ...) #Solve returns the inverse of the matrix which is stored in invFunc
	  x$setInverse(invFunc)
	  invFunc
}

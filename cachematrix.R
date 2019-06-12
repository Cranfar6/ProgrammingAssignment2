## This function creates a special matrix that mitigates the time consuming
##task of matrix inverse by caching the inverse for future use. It sets the value of the matrix, 
##gets the value of the matrix, sets the matrix inverse and gets the matrix inverse 

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

}


## computes the input of the special matrix created above. If the inverse has already been calculated, 
##the inverse should be retrieved from cache.

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

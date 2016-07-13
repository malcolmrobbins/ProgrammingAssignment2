## These functions provide a mechanism for caching a (computationally expensive)
## matrix inverse.  

## This function creates a "cached matrix" that stores the provides matrix and caches 
## its inverse 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of the "cached matrix" create with the
## above function.  The first time it is called it calculates the inverse
## and from then caches its value to avoid having to recalculate it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

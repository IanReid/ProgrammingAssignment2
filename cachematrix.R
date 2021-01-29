## makeCacheMatrix creates an object that wraps a matrix and caches its inverse
## cacheSolve returns the inverse, calculating it only if it is not already 
## cached.

## Returns a list of functions to set and get the values of matrix 'x' and its 
## inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get=get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Uses the functions created by makeCacheMatrix to return the inverse of matrix
## 'x', the cached value if available, otherwise calculates and caches the 
## inverse before returning it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

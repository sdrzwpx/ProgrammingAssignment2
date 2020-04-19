##  Pair of functions that cache the inverse of a matrix

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
        x <<- y
        mi <<- NULL
        }
        get <- function() x
        setmatrixinverse <- function(matrixinverse) mi <<- matrixinverse
        getmatrixinverse <- function() mi
        list(set = set, get = get,
             setmatrixinverse = setmatrixinverse,
             getmatrixinverse = getmatrixinverse)
}


## Compute the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getmatrixinverse()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setmatrixinverse(mi)
        mi
}

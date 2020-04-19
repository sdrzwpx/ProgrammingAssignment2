##  Pair of functions that cache the inverse of a matrix

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL    # a.k.a. matrix inverse
        # "set" function: set the data value
        set <- function(y) {
        x <<- y
        mi <<- NULL
        }
        # "get" function: get the data value
        get <- function() x
        setmatrixinverse <- function(matrixinverse) mi <<- matrixinverse
        getmatrixinverse <- function() mi
        list(set = set, get = get,
             setmatrixinverse = setmatrixinverse,
             getmatrixinverse = getmatrixinverse)
}


## Compute the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        mi <- x$getmatrixinverse()
        # the case that the inverse has already computed
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        # rge case that the inverse has not computed yet
        data <- x$get()
        mi <- solve(data, ...)
        x$setmatrixinverse(mi)
        mi    # return the inverse value
}

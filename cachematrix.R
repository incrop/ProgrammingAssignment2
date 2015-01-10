## These functions can help solving the inverse of the matrix
## and cahe the result to speed up subsequent calls.

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    c <- NULL
    list(
        set = function(y) {
            x <<- y
            c <<- NULL
        },
        get = function() x,
        setCache = function(cache) c <<- cache,
        getCache = function() c
    )
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## Retrieves the inverse from the cache if it has already been calculated.
cacheSolve <- function(x, ...) {
    inv <- x$getCache()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setCache(inv)
    inv
}

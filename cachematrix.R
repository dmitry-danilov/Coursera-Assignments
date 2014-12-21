## Below functions create a special cached "matrix" wrapper object that stores 
## the matrix passed in and cache the inverse of that matrix

## This function stores a matrix and it's inverse and provides a list 
## of getter and setter functions for them

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(inverse) inv <<- inverse
        getsolve <- function() inv
        list(set = set, get = get, 
             setsolve = setsolve, 
             getsolve = getsolve)
}


## This function caches the inverse of a "matrix" wrapper object created by the
## makeCachedMatrix() function. If matrix inverse has not yet been stored, it calculates and stores it,
## otherwise just returns the stored inverse value
 
cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        if (!is.null(inv)) {
                message("getting cached data...")
                return(inv)
        }
        data <- x$get()
        message("cacheSolve called for the first time, executing solve()...")
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}

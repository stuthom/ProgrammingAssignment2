## cachematrix.R
## This file contains 2 functions which allow the creation of a matrix and which can then be used
## to cache the value of the inverse of this matrix, saving time and compute power if this is required 
## frequently for the same matrix.

## makeCacheMatrix - Creates a list of functions to get and set the value of a matrix, and to get and set the
##                   inverse of the matrix.  Resets the inverse whenever a new matrix is created.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(solve) inv <<- solve
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve - Checks to see if we already have a cached copy of the inverse
##              of x. If we do, returns the cached version, alerting the user
##              that they are seeing the cached version. If we don't, solves
##              for the inverse, saves it to the cache, and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}

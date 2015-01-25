## Solutions for second Assignment 
## Coursera RProgramming

## Make "special matrix" that is able to cache inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Return a matrix that is the inverse of 'x'
## function computes inversed matrix or returns
## cached solution if available

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(m)
    inv
}

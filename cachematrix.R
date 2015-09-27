## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create function "makeCacheMatrix" which stores 4 functions
## get() returns the matrix
## set() sets the matrix
## getinv() returns the inverse of the matrix
## setinv() stores the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## Solves the matrix and gives the inversion
## stores the solution

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

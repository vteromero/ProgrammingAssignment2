## Functions to calculate the inverse of a matrix by using a cache system.

## Create a special object to be used to store a matrix and to cache its inverse value.
## Return a list containing the following functions: set, get, setinverse and getinverse.

makeCacheMatrix <- function(x = matrix()) {
    # inverse value (cached)
    inv <- NULL

    # set the value of the current matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # get the value of the current matrix
    get <- function() x

    # set the inverse of the matrix 'x'
    setinverse <- function(inverse) inv <<- inverse

    # get the inverse of the matrix 'x'
    getinverse <- function() inv

    # return a list of functions that belong to the object
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculate the inverse of the special "matrix" x created through the makeCacheMatrix function.
## First checks to see if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the inverse of the data
## and returns its value.

cacheSolve <- function(x, ...) {
    # get the inverse value
    inv <- x$getinverse()

    # if the inverse has already been calculated, return it
    if(!is.null(inv)) {
        message("getting cache data")
        return(inv)
    }

    # calculate the inverse of the matrix with the solve function
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

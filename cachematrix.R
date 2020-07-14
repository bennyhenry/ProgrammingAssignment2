## Two functions: 1) creates a 'special' matrix,
## and 2) a function for calculating the inverse of the matrix - or retrieving
## the inverse if it has already been cached.

## The first function makeCacheMatrix creates a 'special' matrix, which is actually 
## a list of four functions: 1) 'set' the value of the matrix 2) 'get' the
## value of the matrix 3) 'getinverse' of the matrix 4) 'setinverse' of the matrix

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


## This function first checks that the inverse matrix has not already been calculated -
## if it has, then the function gets and returns the cached data; if not, then the 
## function calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

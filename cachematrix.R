## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix and creates a list containing functions that set or get the matrix, and set or get the cached value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve(x)
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function either calculates and returns the inverse of a matrix made by 'makeCacheMatrix' (above) or, if the inverse has already been calculated, returns the cached value.

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

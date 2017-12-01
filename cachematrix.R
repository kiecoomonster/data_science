# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. 

# The first function, makeCacheMatrix creates a special "matrix" object that can
# cache its inverse. It contains a function to:
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The second function, cacheSolve computes the inverse of the special "matrix"
# returned by makeCacheMatrix. If the inverse has already been calculated
# (and the matrix has not changed), then cacheSolve should retrieve the inverse
# from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

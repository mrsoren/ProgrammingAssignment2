## The two functions in this script can be used to compute the inverse of a matrix in a way
## that the result is only actually computed once. 
## The result is cached and will be reused if needed.


## The function 'makeCacheMatrix' constructs an object which can hold a matrix and its inverse
## and has functions to get and set.

makeCacheMatrix <- function(x = matrix()) {
    ## 'x' is the stored matrix
    ## 'm' is the stored inverse matrix
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function 'cacheSolve' takes a CacheMatrix object and returns the inverse matrix. 
## If the CacheMatrix already holds an inverse matric, this will be returned. 
## Otherwise, the inverse matrix will be computed and stored in the CacheMatrix object.

cacheSolve <- function(x, ...) {
    ## Get the inverse matrix (or NULL) from the cache.
    m <- x$getinverse()
    ## If a cached inverse matrix exists, return it.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## Get the (real) matrix.
    data <- x$get()
    ## Compute the inverse matrix.
    m <- solve(data, ...)
    ## Put the inverse matrix in cache.
    x$setinverse(m)
    ## Return the inverse matrix.
    m
}

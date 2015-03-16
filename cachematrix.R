## cacheSolve with helper function makeCacheMatrix gives us a cached alternative to matrix() 
## and solv().
## If the invers matrix have been calculated for a makeCacheMatrix-matrix than cacheSolve will
## return that cached matrix and not solve it again. It is assumed that these functions only will
## be used with inversible matrices.

## makeCacheMatrix takes a matrix as argument and returnes a matrix that can cache the inverse of
## the matrix.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(xi) m <<- xi
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve(x) takes a makeCacheMatrix-matrix as argument and will return the invers matrix of x.
## If the inverse has been calculated for x priviously, than a cached inverse matrix will be 
## returned.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)    
        ## Return a matrix that is the inverse of 'x'
    m
}

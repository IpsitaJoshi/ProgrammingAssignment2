## The functions below compute the inverse of a matrix and then cache it for retrieval if required

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setMatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) { 
        ## If the inverse has already been calculated retrieve the inverse from the cache
        print("Getting cached data")
        return(m)
    }
    ## If the inverse has not been calculated, calculate it by using solve()
    data <- x$getMatrix()
    m <- solve(data,...)
    x$setInverse(m)
    m
}

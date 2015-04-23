## Below are 2 functions ..one to create a matrix that can be reversed, while the 
## second determines the reverse either initially or reusing the cache of a previous
## call.

## This function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        cacheInverse <- NULL
        setMatrix <- function(y)
                x            <<- y
                cacheInverse <<- NULL
        
        getMatrix  <- function() x
        setInverse <- function(answer) cacheInverse <<- answer
        getInverse <- function() cacheInverse
        list(setInverse = setInverse,
             getInverse = getInverse,
             setMatrix  = setMatrix,
             getMatrix  = getMatrix)      
}


## This function computes the inverse of the a "matrix".
## If the inverse has already been calculated (and the matrix has not
## changed), then the function will retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        cacheInverse <- x$getInverse()
        if(!is.null(cacheInverse)) {
                message("getting cache data")
                return(cacheInverse)
        }
        
        Matrix <- x$getMatrix()
        cacheInverse <- solve(Matrix, ...)
        x$setInverse(cacheInverse)
        cacheInverse
}
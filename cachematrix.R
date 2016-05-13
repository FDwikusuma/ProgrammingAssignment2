## Caching the inverse of a square matrix
##

## makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse

makeCacheMatrix <- function(x=matrix()){
        z <- NULL
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) z <<-solve
        getInverse <- function() z
        list(set = set, get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}


## cacheSolve computes the special "matrix" returned by
## makeCacheMatrix. If the inverse has been calculated 
## then the cacheSolve will retrieve the inverse from the
## cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- x$getInverse()
        if(!is.null(z)) {
                message("getting cached data")
                return(z)
        }
        data <- x$get()
        z <- solve(data, ...)
        x$setInverse(z)
        z
}

## The first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(a = matrix()) {
        s <- NULL
        setmatrix <- function(b) {
                a <<- b
                s <<- NULL
        }
        getmatrix <- function() a
        setinv <- function(solve) s <<- solve
        getinv <- function() s
        list(setmatrix = setmatrix,
               getmatrix = getmatrix,
               setinv = setinv,
               getinv = getinv)
}

## The second function computes the inverse of the special "matrix" returned by
## `makeCacheMatrix` above. If the inverse has already been calculated (and the
## matrix has not changed), then `cacheSolve` will retrieve the inverse from
## the cache.

cacheSolve <- function(a, ...) {
        s <- a$getinv()
        if(!is.null(s)) {
                message("returning previously cached data")
                return(s)
        }
        dat <- a$getmatrix()
        s <- solve(dat, ...)
        a$setinv(s)
        s
}
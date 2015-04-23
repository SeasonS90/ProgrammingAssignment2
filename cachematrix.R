## If the contents of a matrix are not changing, it cache the value of the 
## reverse of the matrix so that when we need it again, it can be looked up
## in the cache rather than recomputed.

## makeCacheMatrix:
## get and set the new matrix and its reverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve:
## solve the reverse of the matrix if needed 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
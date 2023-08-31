## Caching the inverse of a matrix

## Returns list of functions for caching the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        # initialize inverse as NULL
        inv <- NULL
        
        # set value of matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # get value of matrix
        get <- function() x
        
        # set value of inverse
        setinv <- function(inverse) inv <<- inverse
                
        # get value of inverse
        getinv <- function() inv
        
        # return list of functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return a matrix that is the inverse of 'x', checks cache first

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mtx <- x$get()
        inv <- solve(mtx)
        message("caching inverse")
        x$setinv(inv)
        inv
}

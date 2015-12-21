## These functions determine the inverse of a matrix if needed and if not pull the result from memory

# makeCacheMatrix establishes the set and get functions for determining the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



# cacheSolve determines whether a new calculation is necessary
# if necessary, the calculation is performed
# if available in memory, the solution is retrieved
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        message("calculating inverse")
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

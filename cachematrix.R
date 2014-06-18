## These functions calculate the inverse of any invertible matrix and cache the result so that if called to recalculate
## it gives the cached result rather than actually recalculating.

## makeCacheMatrix creates a list of functions; set, get, setinverse and getinverse that will create a matrix and calculate 
## its inverse.

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix except when the inverse has been calculated previously in which case it
## will access the previously cached result.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


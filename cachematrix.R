## This is the function code for programming assignment 2 [Coursera course].
## It sets out to write two functions: "makeCacheMatrix" and "cacheSolve".

## makeCacheMatrix will create a special "matrix" object that can cache its inverse.

## The cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


makeCacheMatrix <- function(x = matrix()) {
        ##This function creates a special "matrix" object that can cache its inverse.
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}



cacheSolve <- function(x, ...) {
        ## This function computes the inverse of the special matrix created above.
        ## Return a matrix that is the inverse of 'x'.
        ## If it was already cached, it will get the cached data instead of recalculating it.
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}


## Matrix inversion is often a costly operation and can benefit from caching.
## This library provides a Matrix that caches it's inverse. If the inverse has 
## already been calculated (and the matrix has not changed), then the cachesolve
## function will retrieve the inverse from the cache.

## makeCacheMatrix creates a special matrix object that can cache its inverse.
## This function must be used to take advantage of caching.
##
## param x - If a matrix is provided, it will be wrapped by caching operations.
##           If no x is provided, an empty matrix is instantiated

makeCacheMatrix <- function(x = matrix()) {
        # i holds the inverse, we have not calculated yet so set to null
        i <- NULL
        
        # Set the matrix, x, that is wrapped for caching
        set <- function(y) {
                x <<- y
                # Make sure to set the inverse, i, to null so we know we have to
                # re-compute as the matrix data has changed
                i <<- NULL
        }
        
        # Get the matrix, x, that is wrapped for caching
        get <- function() x
        
        # Set (cache) the inverse of x
        setinverse <- function(inverse) i <<- inverse
        
        # Get the cached inverse of x
        getinverse <- function() i
        
        # Create the list of functions used to perform operations on the matrix.
        # Can get/set the matrix data and get/set the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve will retrieve the inverse from 
## the cache.
##
## param x - The matrix created by makeCacheMatrix
## param ... - These arguments are passed into the solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        ## Return the cached version if one is stored 
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## No cached version. Compute inverse
        data <- x$get()
        i <- solve(data, ...)
        
        ## Store the inverse in the cache and return it
        x$setinverse(i)
        i
}

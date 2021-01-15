## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
## Creates a cache matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL   # Inverse matrix
    
    # Functions
    set <- function(y){
        x <<- y     
        i <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) i <<- solve 
    getsolve <- function() i
    
    # Return
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
## Computes the inverse of a cache matrix object returned by
## makeCacheMatrix. If the inverse has already been calculated,
## the cacheSolve retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getsolve()
    
    if(!is.null(i)){
        message("getting cache data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setsolve(i)
    i
}

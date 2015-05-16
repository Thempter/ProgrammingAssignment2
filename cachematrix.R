## Put comments here that give an overall description of what your
## functions do


## This function will create a special matrix that is able to
## Save in cache it inverse when is setted.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inversein) inverse <<- inversein
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This will return a matrix that is the inverse of 'x'
## IF there's a cached inverse then it will return that 
## Otherwise it will calculate its inverse, cache it on 
## x object and then return it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

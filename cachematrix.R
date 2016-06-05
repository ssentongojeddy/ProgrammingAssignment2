## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation
## There are benefit to caching the inverse of a matrix 
## rather than compute it over and over.
## Here you will find a pair of functions that are used to create a special
## object that stores a matrix and caches its inverse.
## Lets get started!!!

makeCacheMatrix <- function(x = matrix()) {
        
        ## x is a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         The above list of procedures is used as the input to cacheSolve()
        inv <- NULL
        set <- function(y) {
                
                #  the operator `<<-` assigns a value to an object in an environment
                # that is different from the current environment
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        
        ## x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        
        # but if the inverse has already been calculated
        if (!is.null(inv)) {
                
                # then get it from the cache and skips the computation.
                
                message("getting cached data")
                return(inv)
        }
        
        # or else, calculates the inverse 
        data <- x$get()
        inv <- solve(data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinverse(inv)
        
        inv
}
## thanks for following along. Hope you have a nice day!!
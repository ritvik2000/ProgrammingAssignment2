## A pair of functions that cache the inverse of a matrix

## Creatws a matrix that is able to cache it's inverse
## makeCacheMatrix: this function creates a cacheMatrix from the first
##
## The cacheMatrix has the following methods:
##
## cacheMatrix$get(): gets the contents as a regular matrix
## cacheMatrix$set(y): sets the contents to the matrix y, resetting the cache
##
## cacheMatrix$p.getInverse(): gets the cached inverse; NULL if no inverse is set
## cacheMatrix$p.setInverse(new_inv): set the inverse cache

makeCacheMatrix <- function(x = matrix()) {
           i <- NULL
        
        set <- function(y) {
        x <<- y
        i <- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Compute the inverse of the Cached matrix
## This returns the inverse of the cacheMatrix. As long as the matrix has 
## not been changed between calls, successive calls to cacheSolve(m) 
## would not result in re-computation of the inverse (using the intrinsic)
## cache.
##
## Note: all parameters other than x (the cacheMatrix) are ignored given they
## have not been mentioned in the assignment. If we pass them to solve(),
## then we need to check when we retrieve the cache whether they had been 
## the same when the cache was first computed
cacheSolve <- function(x, ...) { 
       
        i <- x$getinverse()
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        
       ## Return a matrix that is the inverse of 'x'
        i
}

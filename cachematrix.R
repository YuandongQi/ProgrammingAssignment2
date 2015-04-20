# It is costly to compute matrix inversion and there may be some benefit
#to caching the inverse of a matrix rather than compute it repeatedly.
#The two functions below can be used to cache the inverse of a matrix.


# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y) {
        x <<- y
        invrs<<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invrs <<- inverse
    getinverse <- function() invrs
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
    
}

x = rbind(c(1, -1/4,3), c(-1/4, 1,2),c(2,3,1))
> m = makeCacheMatrix(x)
> cacheSolve(m)

## The function below compute the inverse of the matrix.
##First, it checks if there exist the inverse. If so, it
##gets the result and no computation will be done.
##If not, it calculate the inverse, sets the value in the cache via
#setinverse function.

cacheSolve <- function(x, ...) {
    invrs <- x$getinverse()
    if(!is.null(invrs)) {
        message("getting cached data")
        return(invrs)
    }
    data <- x$get()
    invrs <- solve(data)
    x$setinverse(invrs)
    invrs
    ## Return a matrix that is the inverse of 'x'
}

x = rbind(c(1, -1/6,3), c(-1/5, 1,2),c(2,3,1))
m = makeCacheMatrix(x)
m$get()
cacheSolve(m) #first run, no cache
cacheSolve(m) # retrieving from the cache in the second run
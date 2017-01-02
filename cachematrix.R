## Below are two functions that are used to create a special object that stores 
## a matrix and cache's its inverse.

## makeCacheMatrix creates a list from a matrix containing a function to
# $ set(): set the value of the matrix
# $ get(): get the value of the matrix
# $ setinverse(): set the value of the inverse
# $ getinverse(): get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve gets the inverse from a cacheMatrix. If the inverse is not 
## currently stored in cache the function computes and stores it via setinverse 
## function. If it is stored, it gets directly using getinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

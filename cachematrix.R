## makeCacheMatrix creates a 'matrix' object that is able to cache its inverse
## cacheSolve takes the 'matrix' created by makeCacheMatrix and computes its 
## inverse. If the inverse has already been calculated, cacheSolve will instead
## retrieve it from the cache.

## creates special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## returns a matrix that is the inverse of 'x' either by computation or
## retrieval from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setinv(inv)
    inv
}
## Functions to create special matrix that supports
## caching the solution of the inverse function

## Create a matrix that supports caching of the inverse
## inv will hold the value of the cached result

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(i) inv <<- i
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Solve the inverse of the matrix, using the cached result if able

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    ## First check to see if a cached value exists
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## otherwise, calculate the inverse, cache it, and return result
    mat <- x$get()
    inv <- solve(mat)
    x$setInv(inv)
    inv
}

## This function creates a "matrix" object that can store matrix+cached version
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL              # init the cached inverse as NULL
    set <- function(y) {     # assigns to new matrix
        x <<- y              # store the new matrix in the parent environment
        inv <<- NULL         # reset inverse
    }
    get <- function() x      # returns the stored matrix
    setinverse <- function(inverse) {
        inv <<- inverse      # Stores inv in parent env
    }
    getinverse <- function() inv  # return cached inverse or null
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}






## This function computes the inverse of the matrix stored in the
## special object created by makeCacheMatrix(), unless its already been cached, in which case we pull from cahce
cacheSolve <- function(x, ...) { 
    inv <- x$getinverse()     # attempt to pull the cached inverse
    if (!is.null(inv)) {      # check if cached value is available
        message("getting cached data")
        return(inv)           # return cached inverse
    }
    data <- x$get()           # retrieve the actual matrix, after no cache found
    inv <- solve(data, ...)   # compute the inverse using solve()
    x$setinverse(inv)         # cache the newly computed inverse
    inv                       # return computed inverse
}

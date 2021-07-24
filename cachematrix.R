## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

mmakeVector <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheinverse <- function(x, ...) {
    i <- x$getinverse()
    # Check to see if i contains any data
    if(!is.null(i)) {
        message("getting cached data")
        # If data is stored, get the cached data.
        return(i)
    }
    # If empty, calculation is done here 
    inv_matrix <- x$get()
    inv <- solve(inv_matrix, ...)
    # Using setinverse function, this command line sets the value of the inverse in the cache     
    x$setinverse(inv)
    # Return the cache inverse
    inv
}

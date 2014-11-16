## Instead of repeatedly calculating the inverse of a square matrix each time 
## it is needed in our project, this function will save that value once calculated
## so we can re-use it quickly.

## This function creates a special matrix object that can cache its inverse.
## First, it sets the value of the matrix.
## Then, it gets the value of the matrix.
## Next, it sets the value of the inverse of the matrix.
## Finally, it gets the value of the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(cacheSolve) m <<- cacheSolve
    getinverse <- function() m
    list(set = set, get = get),
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

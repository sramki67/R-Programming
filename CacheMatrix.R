makeCacheMatrix <- function(x = matrix()) {
    j <- NULL
    set <- function(y) {
        x <<- y
        j <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) j <<- inverse
    getinverse <- function() j
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
    j <- x$getinverse()
    if (!is.null(j)) {
        message("getting cached data")
        return(j)
    }
    data <- x$get()
    j <- solve(data, ...)
    x$setinverse(j)
    j
}

#Computing the inverse of a square matrix can be done with the solve function in R.



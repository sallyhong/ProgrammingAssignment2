## Coursera: Intro to R Programming
## Programming Assignment #2
## Sally Hong

## makeCacheMatrix:
## - creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<-inverse
    getInverse <- function() i
    list(set = set, get = get, 
          setInverse = setInverse, 
          getInverse = getInverse )
}


## cacheSolve:
## - computes the inverse of matrix returned by makeCacheMatrix
## if inverse has already been calculated (and matrix has not
## changed), then the cachesolve should retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}

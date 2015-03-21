## Sometimes we wish to cache potentially time-consuming computations. Today we are going to do this
## for the inverse of a matrix. 03/21/15

## makeCacheMatrix will make a special matrix object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Here we pull the inverse if the matrix has not been changed, if it has, then we recalculate
## the inverse


cacheSolve <- function(x=matrix(), ...) {
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    inverse<-x$get()
    m<-solve(inverse, ...)
    x$setinverse(m)
    m
}

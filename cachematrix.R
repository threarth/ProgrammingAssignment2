## makeCacheMatrix and cacheSolve respectively return a special matrix object which
## contains the 'methods' to set/retrieve its inverse and its data and to compute and
## cache its inverse


## Constructor function which returns a 'matrix' object (a list of the four implemented methods of this object)
## Methods: get, set, getinverse, setinverse
## Members: x (data) and i (inverse)
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL 
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    
    getinverse <- function() i
    
    setinverse <- function(y) i <<- y
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Compute and cache the inverse of the special 'matrix' passed as an argument
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    if (!is.null(i)) {
        message("Getting cached data")
        return(i)
    }

    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

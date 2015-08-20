## makeCacheMatrix and cacheSolve respectively return a special matrix object which
## contains the 'methods' to set/retrieve its inverse and its data and to compute and
## cache its inverse


## return a special matrix object which is actually a list of the four implemented methods
## get x, set x, getinverse, setinverse
## x and i could be thought as a sort of c++ private members
makeCacheMatrix <- function(x = matrix()) {
##  i means inverse; it is the 'private' member var in which we cache our inverse
    i <- NULL 
    
    set <- function(y) {
        x <<- y
##      if data changes, the stored inverse should become NULL; it will be computed later
        i <<- NULL
    }
    get <- function() x
    
    getinverse <- function() i
    
    setinverse <- function(y) i <<- y
    
##  this last expression returns all this 'methods' as a list
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Compute and cache the inverse of the special 'matrix' passed as an argument
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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

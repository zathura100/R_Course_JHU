## These two functions illustrate caching data for a matrix inverse, 
## rather than recalculating the data. They also use lexical scoping,
## making objects defined in one function (makeCacheMatrix) available
## to the other function (cacheSolve).

## makeCacheMatrix stores the matrix in x, initiliazes invr to NULL
## and defines getters and setters. 

makeCacheMatrix <- function(x = matrix()) {
    invr <- NULL
    set <- function(y) {
        x <<- y         ## a new value of x is set with y attribute and
        invr <<- NULL      ## clear any value left in cache by cacheSolve
    }
    get <- function() x
    setinvr <- function(inverse) invr <<- inverse
    getinvr <- function() invr
    list(set = set, get = get,
         setinvr = setinvr,
         getinvr = getinvr)
}

## cacheSolve checks if m contains the current inverse matrix and either
## presents the cached matrix with the message "getting cached matrix" or
## computes the inverse matrix with the message "calculating inverse".

cacheSolve <- function(x, ...) {
    invr <- x$getinvr()
    if(!is.null(invr)) {
        message("getting cached inverse")
        return(invr)
    }
    data <- x$get()
    invr <- solve(data, ...)
    message("calculating inverse")
    x$setinvr(invr)
    invr                   ## Return a matrix that is the inverse of 'x'
}
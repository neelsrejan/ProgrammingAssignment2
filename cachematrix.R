## makeCacheMatrix takes in an invertible square matrix and returns a list of functions.
## setMat setMat caches the passed in data to x and sets the invMat value to Null
## getMat returns the cached x value
## setInvMat sets the invMat variable to the passed in inverse Matrix
## getInvMat returns the inverse Matrix
makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    setMat <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    getMat <- function() x
    setInvMat <- function(inverseMatrix) invMat <<- inverseMatrix
    getInvMat <- function() invMat
    list(setMat = setMat, getMat = getMat,
         setInvMat = setInvMat,
         getInvMat = getInvMat)
}

## cacheSolve takes in the makeCacheMatrix list of functions and uses the getInvMat
## function to see if there is a cached inverse matrix alread present or not. If so we 
## return the cached inverse matrix, else we use getMat to get the data, invserse it using
## the solve() method and then set the inverse Matrix. 
cacheSolve <- function(x, ...) {
    invMat <- x$getInvMat()
    if(!is.null(invMat)) {
        message("getting cached data")
        return(invMat)
    }
    data <- x$getMat()
    invMat <- solve(data, ...)
    x$setInvMat(invMat)
    invMat
}

## Function: makeCacheMatrix
## Description: Creates a special "matrix" object that can cache its inverse.
##  Returns an object of class list, containing the following functions to manage a matrix object:
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse matrix
##    4. get the value of the inverse matrix
## Usage: 
##    my_cacheMatrix = makeCacheMatrix(my_matrix)
## Arguments:
##    x = matrix object (matrix should be an square invertible matrix)
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        ##set the cached matrix to NULL;this is to indicate that the cached value is not available
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    ## return a list object with the functions required to manage the cached matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function: cacheSolve
## Description: This function computes the inverse of an special "matrix" object returned by function makeCacheMatrix. 
##  If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## Usage: 
##    cacheSolve(my_cacheMatrix)
## Arguments:
##    x = special matrix object. An object of class list, containing the following functions to manage a matrix object:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse matrix
##      4. get the value of the inverse matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached inverse matrix data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}

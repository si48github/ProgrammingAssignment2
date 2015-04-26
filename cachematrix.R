## Put comments here that give an overall description of what your
## functions do
## 
## R programming assignment 2 requires a function to cache the inverse of a matrix

## Write a short comment describing this function

## Sunder Iyer comments
# Function makeCacheMatrix follows the same approach as makeVector 
# It takes a matrix as the argument.
# It returns a special "matrix", which is a list containing setter/getter functions to
# - set the value of the matrix
# - get the value of the matrix
# - set the inverse of the matrix
# - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    get <- function() {x}
    setinverse <- function(inverse) {m <<- inverse}
    getinverse <- function() {m}
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function


## Sunder Iyer comments
# Function cacheSolve follows the same approach as cachemean 
# It takes a matrix as the argument and returns the inverse of the matrix
# If the inverse is in the cache, it does not recalculate the inverse
# Otherwise, it calculates the inverse and caches it for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    ## solve takes a matrix a returns the inverse
    # how to handle exceptions when inverse cannot be computed
    # i.e when det(x) is 0 ?
    x$setinverse(m)
    # cache the inverse for future use
    m
}

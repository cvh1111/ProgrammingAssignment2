## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a "vector", list containing a function to
## set the matrix
## get the matrix
## set the matrix inverse (minv)
## get the matrix inverse (minv)

makeCacheMatrix <- function(x = matrix()) {

    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setminverse <- function(solve) minv <<- solve
    getminverse <- function() minv
    list(set = set, get = get,
         setminverse = setminverse,
         getminverse = getminverse)
}


## Write a short comment describing this function
## The function below calculate the inverse of a matrix with makeCacheMatrix. 
## It first checks to see if the inverse is already available. 
##   If so, it gets the inverse from the cache and skips the computation.
##   Otherwise, it calculates the inverse the matrix 
##   and sets the result in the cache via the setminverse function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getminverse()
    ## check is minv is in cache
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    ## if not in cache use solve to reverse matrix
    data <- x$get()
    minv <- solve(data, ...)
    x$setminverse(minv)
    minv
}

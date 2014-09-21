## The two functions 'makeCacheMatrix' and 'cacheSolve' are used to together to store, calculate & retrieve the inverse of a matrix.
## The output of 'makeCacheMatrix' is used as an argument to 'cacheSolve'.

## Function 'makeCacheMatrix' take a matrix x as an argument.
## It contains four sub-functions to: 1) set the matrix, 2) get the matrix, 3) set the inverse of the matrix, and 4) retrieve the inverse of the matrix.

## Sub function 'set' saves the matrix x in the 'makeCacheMatrix' environment (as opposed to the 'set' function environment itself).
## This means that matrix x is available for other sub functions in 'makeCachematrix' to call and retrieve.

## Sub function 'get' retrieves and returns the matrix x.

## Sub function 'setinverse' calculates and saves the inverse (i) of matrix x via the function 'solve'.
## The inverse (i) is saved in the 'makeCacheMatrix' environment, one environment higher than the 'setinverse' function environment itself.
## Again, this allows other functions to retrieve the matrix inverse (i).

## Sub function 'getinverse' returns the matrix inverse (i).

## Finally, 'makeCacheMatrix' returns a list with a reference to each function inside makeCacheMatrix: set, get, setinverse, getinverse.
## This output is possible because the matrix x and inverse i were stored in the 'makeCacheMatrix' environment instead of in the sub-functions.
## This list is used as an input to function 'cacheSolve'.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function 'cacheSolve' determine whether or not the inverse of a matrix has already been calculated.
## The argument to 'cacheSolve' is the output from 'makeCacheMatrix'.
## If the inverse has already been calculated, 'cacheSolve' retrieves and returns the cached value.
## If the inverse has not been calculated, 'cacheSolve' calculates and returned the inverse which is calculated using the 'solve' function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
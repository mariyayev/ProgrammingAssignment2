## Functions makeCacheMatrix and cacheSolve cache the inverse of a matrix.
## 
## x is a square invertable matrix. 
## makeCacheMatrix function 
##  creates a special "matrix" object that can cashe its inverse;
##  returns a list containing functions to
##  1. set the matrix, this function changes the matrix stored in the main function
##  2. get the matrix, this function returns the matrix x stored in the main function
##  3. set the inverse, this function stores the value of the input in a variable inv into the main function
##  4. get the inverse, this function returns the value of a variable inv.
## This list is used as the input to cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        ## use <<- to assign a value to an object in an environment
        ## that is different from the current environment
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve function computes the inverse of the special "matrix" returned by
## makeCacheMatrix function. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve function retrieves its value. 
## Otherwise the inverse is calculated and gets stored in the object generated
## assigned with makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv) 
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    return(inv)
}
## Two R functions for solving the inverse of (potentially large)
## matrices by retrieving cached results, if the inverse has already been solved,
## or by solving then storing the inverse in a special type of matrix.
##
## Based on the templates 'makeVector' and 'cachemean' described by RPeng in 
## the README.md located in this forked repo or in the original repo at 
## https://github.com/rdpeng/ProgrammingAssignment2 .
## 
## Submitted in partial completion of 'Programming Assignment 2" for the  
## course 'R Programming' as part of the Data Science Track on Coursera
## Submitted by: epigenus
## Section: rprog-005
## Date: August 2014


## Function: makeCacheMatrix(x)
## Creates a new type of matrix object that stores its own inverse (if the inverse 
## has already been solved for) and provides functions for solving, caching, and 
## retrieving the inverse of itself.
## It takes as input a matrix and outputs a list of factors available for the
## new matrix object.

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y)
        x <<- y
        inv <<- NULL
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}

## Function: cacheSolve(x, ...)
## Returns the inverse of a matrix by either retrieving the solution of a previously 
## solved matrix or by calculating and storing the solution as a cached matrix.
## It takes as input a matrix 'x' and additional arguments for the solve function
## and outputs the inverse of 'x'.

cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

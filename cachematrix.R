## Two R functions for creating and working with a special matrix object which 
## can store a matrix and it's inverse together.  By storing it as one object 
## this potetnially saves repetition of computing time required for calculating
## the inverses of matrices.
##
## Based on the templates 'makeVector' and 'cachemean' described by RPeng in 
## the README.md located in this forked repo or in the original repo at 
## https://github.com/rdpeng/ProgrammingAssignment2 .
## 
## Submitted in completion of 'Programming Assignment 2" for the  
## course 'R Programming' as part of the Data Science Track on Coursera
## Submitted by: epigenus
## Section: rprog-005
## Date: July 2014


## Function: makeCacheMatrix(x)
## Creates a new type of matrix object that stores its own inverse (if the inverse 
## has already been solved for) and provides functions for solving, caching, and 
## retrieving the inverse of itself.  Upon creation it stores the matrix itself and NULL
## for the inverse.
## It takes as input a matrix and outputs a list of function factors available to 
## manipulate the new object matrix object.

makeCacheMatrix <- function(x = matrix()) {
    
    #set initial inverse to NULL (as in not yet solved)
    inv <- NULL
    # set the values of the matrix (can reset as well)
    set <- function(y)
        x <<- y
        inv <<- NULL
    # a function to retrieve the value of the matrix from this special object
    get <- function() x
    # a function to solve and set the inverse of the matrix
    setinv <- function(solve) inv <<- solve
    # a function to retrieve the invese of the matrix from this special object
    getinv <- function() inv
    # the output list of functions available from this object
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}

## Function: cacheSolve(x, ...)
## Returns the inverse of the special matrix object created by the makeCacheMatrix 
## function. The function does this by either retrieving the solution of a previously 
## solved matrix, or by calculating and storing the solution in the cached matrix object.
## It takes as input a cahced matrix 'x' and additional arguments for the solve function
## and outputs the inverse of 'x'.

cacheSolve <- function(x, ...) {
    
    # retrieve the value of the inverse of a cache matrix
    inv <- x$getinv()
    # if the inverse has been calculated already, don't waste computing time
    # by solving it again.  Just used the stored value.
    if(!is.null(inv)){
        message("getting cached inverse")
        return(inv)
    }
    # Otherwise, get the matrix...
    data <- x$get()
    # solve for the inverse...
    inv <- solve(data, ...)
    # and store that inverse in the object for future reuse
    x$setinv(inv)
    # Finally, Output the inverse of the matrix
    inv
}

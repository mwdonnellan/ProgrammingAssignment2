## R language source code file: cachematrix.R
## Author:      mwdonnellan
## Date:        May 24, 2015
##
## Purpose:     written to satisfy the Programming Assignment 2 requirement
##              for Coursera Data Science Specialization course RProg-014,
##              "R Programming."
##
## Contents:    
## function makeCacheMatrix:       
## Constructor for a "square matrix object" (actually a list) 
## that can store both a square matrix and (optionally) its inverse. 
## A CacheMatrix caches the inverse of the matrix 
## supplied as the argument to makeCacheMatrix. 
## The inverse matrix is calculated and stored by function cacheSolve.
##
## function cacheSolve: 
## Function to calculate and store
## the inverse of the original square matrix stored in a
## CacheMatrix.  That inverse matrix is itself also stored in the
## CacheMatrix object (list).
##
## Based on:    "Caching the Mean of a Vector" example code in the instructions for 
##              Programming Assignment 2.

## makeCacheMatrix builds and returns a CacheMatrix "object" that is
## in its essence a list, containing functions to set and get the original
## matrix passed as an argument/parameter to makeCacheMatrix, and to
## set and get the inverse of that original matrix.  The set() and setinverse()
## functions use the <<- operator to make assigment take place in the global
## environment (assuming that, in this case, x and mi are not defined in
## any parent environment; if so, they are redefined here.)  This ensures
## the visibility of x and mi to externally-defined functions.

makeCacheMatrix <- function(x = matrix()) 
{
        # initialize storage for inverse of input matrix
        mi <- NULL
        
        # set the original matrix to the matrix passed as argument 'y'
        # set the inverse matrix to NULL, using the <<- operator
        set <- function(y)
        {
                x <<- y
                mi <<- NULL
        }
        
        # return the original matrix
        get <- function() x 
        
        # set the inverse matrix to the matrix passed as argument 'inverse'
        setinverse <- function(inverse) mi <<- inverse
        
        #return the inverse matrix
        getinverse <- function() mi
        
        
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function requires an argument that exposes get, set,
## getinverse() and setinverse() functions, such as is returned by function
## makeCacheMatrix above. cacheSolve assumes that the matrix returned by the
## call to x$get() is invertible, and does not include error-handling.
## cacheSolve calls solve to calculate the inverse of the matrix returned by the
## call to the get() member of its input argument.

cacheSolve <- function(x, ...) 
{
        ## put the result of the call to the getinverse() member of the
        ## input argument into i
        i <- x$getinverse()
        
        ## if i is not NULL, return the inverse matrix that has already been
        ## calculated and stored
        if (!is.null(i)) 
        {
                message ("getting cached data")
                return(i)
        }
        
        ## if we got here, i was NULL,so get the original matrix, calculate its
        ## inverse, and store the inverse calling the setinverse() member of the
        ## input argument.
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        
        ## return the newly-calculated inverse matrix
        return(i)
}

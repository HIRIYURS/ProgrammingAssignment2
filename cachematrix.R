## This R file defines the following functions:
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache 
##                     its inverse. It creates a list containing the following functions
##                      - set the Matrix
##                      - get the Matrix
##                      - set/cache the Inverse of Matrix
##                      - get the Inverse of the Matrix
##
## 2. cacheSolve:      This function computes the inverse of the special "matrix" 
##                     returned by makeCacheMatrix function. If the inverse has already
##                     been calculated (and the matrix has not changed), then the 
##                     cachesolve should retrieve the inverse from the cache.
##
## The R function solve() is used to Compute the inverse of a square matrix 
##
## For this assignment, It is assumed that the matrix supplied is always invertible.


## Function: makeCacheMatrix - This function creates a special "matrix" object that can 
##   cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    mcinverse <- NULL

    set <- function(y=matrix()) {
        x <<-y
        mcinverse <<-NULL
    }

    get <- function() x

    setinverse <- function(linverse) mcinverse<<-linverse

    getinverse <- function() mcinverse

    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Function: cacheSolve - This function computes the inverse of the special "matrix"
##   returned by makeCacheMatrix function defined above. If the inverse has already been
##   calculated (and the matrix has not changed), then the cachesolve should retrieve
##   the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mtInv <- x$getinverse()

    ## Check if the Inverse is already calculated
    if(!is.null(mtInv)) {
        print("Return the cached data")
        return(mtInv)
    }
    
    ## Get the matrix data to compute the Inverse
    data <- x$get()
    
    ## Useth solve function in R to compuet the inverse of the matrix
    mtInv <- solve(data,...)
    
    ## Cache the computed inverse of the matrix
    x$setinverse(mtInv)
    
    ## Return the Inverse
    mtInv
}

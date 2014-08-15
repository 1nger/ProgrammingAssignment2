# R-Programming Assignment 2:

# Write the following functions:
        
    #1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
    
    #2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
        #above. If the inverse has already been calculated (and the matrix has not changed), then the
        #cachesolve should retrieve the inverse from the cache.

# Computing the inverse of a square matrix can be done with the solve function in R. For example,
# if X is a square invertible matrix, then solve(X) returns its inverse.

# For this assignment, assume that the matrix supplied is always invertible.

_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _


## Put comments here that give an overall description of what your functions do.


## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv.m <- NULL
        set <- function(y) {
                x <<- y
                inv.m <<- NULL
        }
        get <- function() x
        set.inv <- function(inverse) inv.m <- inverse
        get.inv <- function() inv.m
        list(set = set, get = get, setinverse = set.inv, getinverse = get.inv)
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv.m <- x$getinverse()
        if (!is.null(inv.m)) {
                print("getting cached inverse of matrix")
                return(inv.m)
        }
        my_matrix <- x$get()
        inv.m <- solve(my_matrix)
        x$setinverse(inv.m)
        inv.m
}

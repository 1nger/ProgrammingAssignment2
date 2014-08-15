# R-Programming Assignment 2:


# This set of two functions can store the values of a matrix and its inverse (if the matrix is invertible)
# in a variable in the global environment, and can then later retrieve them from there,
# rather than calculating it again.


# 1. makeCacheMatrix: This function creates a special list, which contains 4 functions.
# These functions can A) cache a matrix (save it), B) return this cached matrix, C) set the value of a variable which stores the inverse
# of the cached matrix and D) return the inverse of the matrix from that variable.

makeCacheMatrix <- function(x = matrix()) {   # defines function, input is a matrix.
        inv.m <- NULL                         # starts by setting the variable "inv.m" to empty, in the local environment (the function environment).
        set <- function(input) {              # 1st function can assign matrix data to a variable called "x" and create an empty variable
                                              # called "inv.m" (where the inverse of the matrix will be stored), both in the global environment.
                x <<- input
                inv.m <<- NULL
        }
        get <- function() x                   #2nd function can return the value of the input matrix.
        set.inv <- function(inverse) inv.m <<- inverse   #3rd function assigns a value to the variable "inv.m" in the global environment.
        get.inv <- function() inv.m           #4th function returns the value, which is currently stored in the variable "inv.m".
        list(set = set, get = get, setinverse = set.inv, getinverse = get.inv)
        #above: finally a list is created, which holds these 4 functions, from where they can be called by name with the $ operator
        #because it is the last statement in the function, this is what is returned when the function is called.
}


# 2. cacheSolve: This function first checks to see if there is an inverse matrix cached already ("inv.m" is not empty/NULL).
# if a value is stored there (the inverse of our matrix), then it is returned together with a message stating this,
# and otherwise (if inv.m is empty), then it fetches the cached input matrix via the list (get()) and calculates the inverse using
# the solve() function. It then stores the inverse matrix in "inv.m" (with setinverse()) and returns it.

cacheSolve <- function(xlist) {         # defines function (input should be the list returned by makeCacheMatrix()).
        inv.m <- xlist$getinverse()     # fetches the value of the global "inv.m" via the getinverse() function from the list and stores it locally in "inv.m"  
        if (!is.null(inv.m)) {          # checks to see if anything is already stored in "inv.m"
                message("retrieving cached inverse of matrix")  #and if something is stored there is returns it
                return(inv.m)                                   #together with a message saying so. The return() statement causes the function to be exited at this point.
        }
        my_matrix <- xlist$get()        # If "inv.m" was empty/NULL, then it continues with the code and fetches the input matrix,
        inv.m <- solve(my_matrix)       # calculates the inverse with solve() and then stores the inverse matrix in "inv.m". 
        xlist$setinverse(inv.m)         # It then uses the setinverse() function to store this inverse matrix in "inv.m" in the global environment
        inv.m                           # and finally returns the inverse matrix (with no message)
}

## Thanks for reading my code, I hope it was all clear. Have a nice day! :)

## This file has two functions which purpose is to 
## cache the inverse of a matrix

## The first function, "makeCacheMatrix" creates a special "matrix" object
## that can cache its inverse. This function is really a list containing a
## function to:
## 1. set the value of a matrix (set)
## 2. get the value of a matrix (get)
## 3. set the value of a inverse matrix (setInverse)
## 4. get the value of a inverse matrix (getInverse)

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize the variable "i" as NULL
        i <- NULL
        
        ##  Sub-function "set", which set the value of a matrix
        set <- function(y) {
                
                ## sets "x" to "y" within the whole function and initialize
                ## "i" as NULL
                x <<- y
                i <<- NULL
        }
                
        ## Sub-function "get", which reports the value of "x"
        get <- function() x
        
        ## Sub-function "setIverse", which uses the function solve to set the 
        ## inverse matrix.
        setInverse <- function(solve) i <<- solve      
                
        ## Sub-function "getInverse", which reports the inverse matrix
        getInverse <- function() i
        
        ## Create the list of functions
        list(set = set, get = get,
             setInverse = setInverse,getInverse = getInverse)
}


## The function "cacheSolve" computes the inverse of the special "matrix"
## object returned by "makeCacheMatrix".

cacheSolve <- function(x, ...) {
        
        ## Defines "i" as the inverse matrix of "x"
        i <- x$getInverse()
        
        ## Checks to see if the inverse matrix has already been calculated. 
        ## If so, get the inverse matrix from the cache 
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## If the inverse matrix has not been calculated, sets the value of
        ## the inverse matrix of the data in the cache via the setInverse
        ## function.
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
        
}

# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the
# matrix. Contains the following functions:
# * setMatrix set the value of a matrix
# * getMatrix get the value of a matrix
# * setInverse get the cached value (inverse of the matrix or i)
# * getInverse get the cached value (inverse of the matrix or i)
#


makeCacheMatrix <- function(x = matrix()) { # input x will be a matrix
        
        i <- NULL # i will our 'inverse' that will be cached, 
                  # we set it to NULL every time the function makeCacheMatrix is called
        
        
        
        setMatrix <- function(y) { # takes an input matrix
                x <<- y            # saves the input matrix 
                i <<- NULL         # resets the inverse to NULL, basically what happens when a new object is generated.
        }
        
        getMatrix <- function() x # this function returns the value of the original matrix
        
        setInverse <- function(solve) i <<- solve
        # this is called by cacheSolve() during the first cacheSolve()
        #  access and it will store the value using superassignment
        
        getInverse <- function() i
        # this will return the cached value to cacheSolve() on
        #  subsequent accesses
        
        list(setMatrix = setMatrix,     # this is accessed each time makecachedMatrix() is called, 
             getMatrix = getMatrix,     #  that is, each time we make a new object.  This is a list of the internal functions ('methods') 
             setInverse = setInverse,   # so a calling function knows how to access those methods.
             getInverse = getInverse)
             
             

}


# The following function calculates the inverse of a "special" matrix created with
# makeCacheMatrix

cacheSolve <- function(x, ...) { # the input x is an object created by makeCacheMatrix
        
        i <- x$getInverse()  # accesses the object 'x' and gets the value of the inverse of the matrix
        
        
        
        if(!is.null(i)) {                      # if inverse of the Matrix was already cached (not NULL) ...
                
                message("getting cached data") # ... send this message to the console
                return(i)                      # ... and returns the inverse, the "return" ends 
        }
        
          
        data <- x$getMatrix()       # we reach this code only if x$getInverse() returned NULL
        i <- solve(data, ...)   # if i was NULL then we have to calculate the inverse
        x$setInverse(i)             # store the calculated inverse value in x 
        i                           # return the inverse to the code that called this function
}

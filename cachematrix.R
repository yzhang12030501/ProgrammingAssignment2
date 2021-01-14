## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

rm(list=ls()) # remove all objects at once

makeCacheMatrix <- function(x = matrix()) { 
        
        inv <- NULL       
        
        set <- function(y) {
                x <<- y  # superassignment operator: <<-; assign y to x in the upper-level environment which is environment where function is defined (global)
                inv <<- NULL  
        }
        
        get <- function()  
                x       
        
        setinverse <- function(inverse) 
                inv <<- inverse  # assign inverse to inv in the upper environment which is the body of makeCacheMatrix function
        # not the global environment in this case
        getinverse <- function() 
                inv   # get the inversed matrix
        
        list(set = set, get = get,   # makeCacheMatrix returns a list of functions
             setinverse = setinverse,       
             getinverse = getinverse)
}


cacheSolve <- function(x) { # x is a parameter, which is a list of functions
        inv <- x$getinverse()
        # if inverse is already being computed 
        
        if(!is.null(inv)) { # if m is not NULL
                message("getting cached inversed matrix")
                return(inv)
        }
        
        # if inverse is not being computed; or inv is NULL
        
        matrix <- x$get()  # get the matrix from  makeCacheMatrix
        inv <- solve(matrix) # compute the inverse of matrix
        x$setinverse(inv) # assign inv to inv
        inv # return inv
}

# Test 1 --- matrix(c(1,2,3,4))
matrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
matrix # this return a list of function defined in the makeCacheMatrix
matrix$get() # check the matrix
cacheSolve(matrix) # compute the inverse, and this is stored in inv through setinverse()

matrix$getinverse() # get the inverse

cacheSolve(matrix) # call this function again would return the cached inversed matrix, since it is already computed.

# Test 2 --- matrix(c(0,5,99,66))
matrix = makeCacheMatrix(matrix(c(0,5,99,66), nrow=2, ncol=2))
matrix$get()
cacheSolve(matrix)

matrix$getinverse() 

cacheSolve(matrix)

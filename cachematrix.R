##  We have two complementary functions:
##  makeCacheMatrix - defines methods required for retrieving
##                    and storing a cached value of the inverse
##                    of a matrix
##  cacheSolve      - returns the inverse of a matrix, from 
##                    a cached value if available
##  
##  Given an inversable matrix 'x' the makeCacheMatrix function will
##  return an object 'z' containing a list of methods that when  
##  passed to the function cacheSolve will allow to persist the value of
##  the inverse of the given matrix and return it upon calling
##  or set it if not available


#################################################
##  Function:   makeCacheMatrix 
##  Arguments:  x - matrix type
##  
##  This function takes a matrix object as input 
##  and returns a list of functions


makeCacheMatrix <- function(x = matrix()) {
    # initializes "m" variable to NULL
    # "m" will contain the cached value of 
    # the inverse of matrix "x"
    m <- NULL
    
    # the "set" function uses superassignment to replace the 
    # value of x with that of y
    # it then initializes the cached value "m" since it's no
    # longer valid 
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # function "get" returns the value of the original matrix
    get <- function() x
    
    # function "setinv" will set "m" to be the inverse of matrix
    # "x" using the solve function
    setinv <- function(solve) m <<- solve
    
    # function "getinv" returns the current value of "m"
    getinv <- function() m
    
    # this list contains all the functions that are part of the
    # newly created object
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Write a short comment describing this function

#########################################################
##  Function:   cacheSolve 
##  Arguments:  x - makeCacheMatrix type
##  
##  This function takes a makeCacheMatrix object as input 
##  and returns the inverse of the matrix given

cacheSolve <- function(x, ...) {
    
    # get the cached value of 'x' if available
    m <- x$getinv()
    
    # if 'm' is not null we return the cached value
    # and exit
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # if 'm' is null then we get the value of the
    # original matrix
    data <- x$get()
    
    # and then we get its inverse using 'solve' function
    m <- solve(data, ...)
    
    # then we set the cached value 'm' to the newly
    # calculated inverse
    x$setinv(m)
    
    # finally... we return the inverse of original 
    # matrix 'x'
    m    
}

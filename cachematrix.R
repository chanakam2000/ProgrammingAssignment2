# Following functions are  to calculate inverse of a matrix
# Once calculation of inverse is done, the inverse is cached.
# In subsequent attemps of calculation of inverse, the cached value is used 
# rather than computing it again.


## This function is to create a special matrix .
#The function return list of functions to set and get value of matrix and inverse of it

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        
        get <- function() x
        
        set_inverse <- function(inverse) inv_x <<- inverse
        
        get_inverse <- function() inv_x
        
        list(set = set, 
             get = get, 
             set_inverse = set_inverse, 
             get_inverse = get_inverse)
        
}


## This function is to calculate inverse of a matrix
# If the inverse is cached it used cached value rather than calculating inverse

cacheSolve <- function(x, ...) {
        inv_x <- x$get_inverse()
        
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        else {
                x <- x$get()
                inv_x <- solve(x, ...)
                x$set_inverse(inv_x)
                return(inv_x)        
        }

}

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
        
        matrix <- x$get()
        inv_x <- solve(matrix, ...)
        x$set_inverse(inv_x)
        return(inv_x)        


}

# Sample run
# > x = rbind(c(1, -1/4), c(-1/4, 1))
# > m = makeCacheMatrix(x)
# > m$get()
# [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00
# > cacheSolve(m)
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# > cacheSolve(m)
# getting cached data
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
##  MakeCacheMatrix
##  Creates a special vector (list) containing functions which:
##      1.set the value of a square matrix
##      2.get the value of a square matix
##      3.set the inverse of a square matrix
##      4.get the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
    # When makeCacheMatrix is called, 'x' contains the original matrix and its 
    # inverse matrix 'Inv_m' is set to NULL (it has not yet been calculated).
    Inv_m <- NULL
    
    set_m <- function(y) {
        # Re-assign matrix 'x' with the values of matrix 'y' and clear the cache.
        # Since 'x' and 'Inv_m' were declared in the parent environment of this function, you
        # must use the <<- assignment to correctly access them. 
        x <<- y
        Inv_m <<- NULL
    }
    get_m <- function() {
        # Return the original matrix.
        # Since 'x' was not defined in this function, R searches its parent environment and finds 'x'there.
        x
    }
    setInv_m <- function(i){
        # Cache the inverse matrix using the values from 'i'.
        # The '<<-' assignment operator ensures 'Inv_m', declared in the parent environment, is assigned.
        Inv_m <<- i
    }
    getInv_m <- function() {
        # Return the cached inverse matrix, whether it is NULL or not.
        Inv_m
    }
    
    # Create and return a list of the above functions
    list(set_m = set_m, get_m = get_m,
         setInv_m = setInv_m,
         getInv_m = getInv_m)
}


##  cacheSolve
##  Calculates the inverse matrix of the special vector (list) created with 'makeCacheMatrix'.
##      It first checks to see if the inverse has already been calculated.
##      If so, it gets the inverse from the cache and skips the computation.
##      Otherwise, it calculates the inverse of the matrix and sets value of the inverse in the cache.

cacheSolve <- function(x = matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Get the cached inverse matrix
    i_m <- x$getInv_m()
    
    # Return the cached inverse matrix if it is not NULL
    if(!is.null(i_m)) {
        message ("retrieved cached data")
        return(i_m)
    } 
    
    # Since the cached inverse matrix is NULL, we must retrieve the original matrix,
    # and then calculate, cache and return its inverse.
    data <- x$get_m()
    i_m <- solve(data, ...)
    x$setInv_m(i_m)
    i_m
}





## CODE FOR TESTING IS BELOW:

# Set the original matrix and return the special list of functions
m_list <- makeCacheMatrix(matrix(c(2,7,-5,3), nrow=2, ncol=2))

# Return original matrix
m_list$get_m()

# Return the calculated inverse matrix
cacheSolve(m_list)

# Return the cached inverse matrix
cacheSolve(m_list)

# Return the inverse matrix
m_list$getInv_m()

# Reset the orginal matrix
m_list <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))

# Return original matrix
m_list$get_m()

# Return the calculated inverse matrix
cacheSolve(m_list)

# Return the cached inverse matrix
cacheSolve(m_list)

# Return the inverse matrix
m_list$getInv_m()

# Store the original matrix
m0 <- m_list$get_m()

# Set the inverse matrix to the original matrix
m_list$setInv_m(m0)

# Now the original and inverse matrices are the same (which is a problem - don't use setInv_m outside of 'cacheSolve')
m_list$get_m()
m_list$getInv_m()

# Reset the orginal matrix to a 4 by 4 and calculate its inverse
m_list <- makeCacheMatrix(matrix(c(9,1,3,6,13,11,7,0,5,7,4,7,2,6,1,10), nrow=4, ncol=4))
cacheSolve(m_list)

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
        
        message("re-assigning the original matrix and clearning its cached inverse")
        x <<- y
        Inv_m <<- NULL
    }
    get_m <- function() {
        # Return the original matrix.
        # Since 'x' was not defined in this function, R searches its parent environment and finds 'x'there.
        
        message("returning original matrix")
        x
    }
    setInv_m <- function(i){
        # Cache the inverse matrix using the values from 'i'.
        # The '<<-' assignment operator ensures 'Inv_m', declared in the parent environment, is assigned.
        
        message("setting inverse matrix cache")  
        Inv_m <<- i
    }
    getInv_m <- function() {
        # Return the cached inverse matrix, whether it is NULL or not.
        
        #message("returning the cached inverse matrix - NULL or !NULL")  
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
        message("returning cached (non-null) inverse matrix")
        
        return(i_m)
    }
    
    # The cached inverse matrix is NULL, therefore retrieve the original matrix,
    # calculate, cache and return its value.
    
    message("retrieving original matrix; calculating, caching and returning its inverse matrix")
    
    data <- x$get_m()
    i_m <- solve(data, ...)
    x$setInv_m(i_m)
    i_m
}

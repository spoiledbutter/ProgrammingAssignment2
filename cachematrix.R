## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function returns a matrix that saves the inverse in a variable.
# When the matrix is changed, the inverse needs to be recalculated. The 
# matrix has functions to get and set the matrix values and functions 
# to get and set the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    cached_inverse = NULL
    set <- function(new_matrix)
    {
        x <<- new_matrix
        cached_inverse <<- NULL
    }
    get <- function()
    {
        x
    }
    
    setinverse <- function(new_inverse)
    {
        cached_inverse <<- new_inverse
    }
    getinverse <- function() 
    {
        cached_inverse
    }
    list(set = set, 
         get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# This function will take a CacheMatrix, which is created by the above 
# function, and return the inverse of that matrix. If the inverse was 
# already calculated, it gets it from the CacheMatrix. If the inverse
# hasn't been calculated, it calculates it, saves it in the CacheMatrix, 
# and returns in to the caller.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrix_inverse <- x$getinverse()
    if(!is.null(matrix_inverse))
    {
        return(matrix_inverse)
    }
    matrix <- x$get()
    matrix_inverse <- solve(matrix)
    x$setinverse(matrix_inverse)
    return(matrix_inverse)
}

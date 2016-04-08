## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
##==============================================================================
## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse. This does the below 4 functionalities.
##      set the value of the Matrix as 'x'
##      get the value of the Matrix 'x'
##      set the value of the inverse of Matrix (x) as 'inverse'
##      get the value of the inverse Matrix 'inverse'
##
## Usage: special_matrix <- makeCacheMatrix(some_square_matrix)
##              where   special_matrix is some assignment variable

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    set_inverse_matrix <- function(inverse_matrix) inverse <<- inverse_matrix
    get_inverse_matrix <- function() inverse
    list(set = set, get = get,
         set_inverse_matrix = set_inverse_matrix,
         get_inverse_matrix = get_inverse_matrix)
}
##==============================================================================
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve will retrieve the 
## inverse from the cache.
##
## Usage: cacheSolve(special_matrix)
##          Where special_matrix is the output of makeCacheMatrix function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$get_inverse_matrix()
    if(!is.null(inverse)) {
        message('Getting data from the cache')
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$set_inverse_matrix(inverse)
    inverse
}

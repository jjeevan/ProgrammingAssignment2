## cachmatrix.R contains 2 functions. The first one 'makeCacheMatrix' creates functions for setting value of a matrx and caching the inverse of the matrix.
## The second function 'cacheSolve' retrieves the inverse of the matrix from 'makeCacheMatrix'

## makeCacheMatrix() is a parent function which contains a set of functions and returns the functions as a list. It 'sets' and 'gets' the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {                      ## create parent envt. for a matrix 'x'
        inv <- NULL                                              ## create a NULL object to store inverse of matrix later
        set <- function(y) {                                     ## this function sets a function(y)
                x <<- y                                          ## assigns y to x in the parent envt. 
                inv <<- NULL                                     ## assigns NULL to 'inv' in the parent envt                                                    
                
        }
                get <- function() x                              ## this function calls x from the parent envt.
                setInverse <- function(inverse) inv <<- inverse  ## this function assigns inverse value to 'inv' in the parent envt.
                getInverse <- function() inv                     ## this function 'gets' the inverse value stored as 'inv' in the parent envt.
                list(set = set,                                  ## returns all the functions within the parent envt. as a list of 'set', 'get', 'setInverse' and 'getInverse'
                     get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
        
}


## cacheSolve retireves the inverse of matrix x from makeCacheMatrix(), asuming all are square invertible matrices.

cacheSolve <- function(x, ...) {                                 
        inv <- x$getInverse()                                    ## 'inv' calls only the 'getInverse' variable from the list returned from makeCacheMatrix(x)
        if (!is.null(inv)) {                                     ## if inverse of matrix is not a null value
                message("getting cached data")                   ## it displays, 'getting cached data' 
                return(inv)                                      ## and returns 'inv'.
        }
        mat <- x$get()                                           ## 'mat' calls only the matrix from 'get' variable from makeCacheMatrix(x)
        inv <- solve(mat, ...)                                   ## solve() computes the inverse of square matrix 
        x$setInverse(inv)                                        ## assigns the inverse of martix in 'setInverse' of the function x
        inv                                                      ## 'inv' in the parent envt. returns the inverse of matrix.
}
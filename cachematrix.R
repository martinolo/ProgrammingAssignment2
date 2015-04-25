## The following functions create a special object that stores a matrix
## and caches its inverse matrix.

## This function creates a special "matrix" object that can cache its inverse.
## It creates a list containing a function to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix 'm_inv'
## - get the value of the inverse matrix 'm_inv'

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        ## This if/else structure checks that the matrix is square.
        if(nrow(x) != ncol(x)) {
                print("Warning: invalid matrix! Matrix must be square")
        } else {
                set <- function(y) {
                        x <<- y
                        m_inv <<- NULL
                }
                get <- function() x
                setinv <- function(solve) m_inv <<- solve
                getinv <- function() m_inv
                list(set = set, get = get,
                     setinv = setinv, getinv = getinv)
        }
}


## This function computes the inverse of the special "matrix" returned by
## the function makeCacheMatrix. If the inverse has already been calculated
## and the matrix has not changed, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets its value in the cache using the 'setinv' function.

cacheSolve <- function(x, ...) {        
        m_inv <- x$getinv()
        if(!is.null(m_inv)) {
                message("getting cached data")
                return(m_inv)
        }
        data <- x$get()
        ## This if/else structure controls that the matrix is invertible,
        ## that is, its determinant is not equal to zero (nonsingular matrix).
        if(det(data) != 0) {
                ## Return a matrix 'm_inv' that is the inverse of 'x'.
                m_inv <- solve(data, ...)
                x$setinv(m_inv)
                ## Round the values of the matrix to 6 significant digits.
                round(m_inv, digits = 6)
        } else {
                print("The determinant is equal to zero. Matrix not invertible")
        }
}

## Put comments here that give an overall description of what your
## functions do

## Makes a special "matrix" object that can cache its inverse
## Args:
##     x: matrix to use to construct the cache matrix
## Returns:
##     Special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    get <- function() x
    get_inv <- function() inverse
    set <- function(new_x) {
        x <<- new_x
        inverse <<- NULL
    }
    set_inv <- function(new_inverse) {
        inverse <<- new_inverse
    }
    list(set = set, get = get, set_inverse = set_inv, get_inverse = get_inv)
}

## A special version of solve() that accepts caching matrix arguments
## Args:
##     x: the caching matrix to compute the inverse for
##     ...: arguments to pass along to intrinsic solve()
## Returns:
##     The inverse for x

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$get_inverse()
    if (!is.null(inverse)) {
        return(inverse)
    }
    matrix_x <- x$get()
    inverse <- solve(matrix_x, ...)
    x$set_inv(inverse)
    inverse
}

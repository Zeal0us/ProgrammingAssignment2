##Create and solve on a cacheable matrix object

## Creates and returns a cachable matrix from an R matrix from the matrix function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(new_value) {
                x <<- new_value
                inverse <<- NULL
        }
        get <- function() x
        set_inverse <- function(new_inverse) inverse <<- new_inverse
        get_inverse <- function() inverse
        invisible(list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse))
}


## returns the result of solve on a given cacheable matrix from makeCacheMatrix and stores the result
## If the answer is already cached, it is stored for future runs.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()
        if(!is.null(inverse)) {
                return(inverse)
        }
        inverse <- solve(x$get(), ...)
        x$set_inverse(inverse)
        inverse
}

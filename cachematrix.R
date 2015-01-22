
# DESCRIPTION:
# Wraps a matrix in a data structure that holds its invers along with the 
# matrix itself.
#
# EXAMPLE:
# > m <- matrix(1:4, 2, 2)
# > c <- makeCacheMatrix(m)
# > c$getmat()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > c$setmat(matrix(rep(1,4),2,2))
# > c$getmat()
#      [,1] [,2]
# [1,]    1    1
# [2,]    1    1
makeCacheMatrix <- function(x = matrix()) {
    # the cache for the invers
    cache <- NULL
    
    # create a vector of two pairs of functions to set and get the matrix
    # and the cached inverse of the matrix, respectively
    accessors <- list(
        setmat = function(m) {
            x <<- m
            cache <<- NULL
        }, 
        getmat = function() 
            x,
        setinv = function(inv) 
            cache <<- inv,
        getinv = function() 
            cache
    )
    # give the vector a distinct class so that we can make sure we are dealing
    # with the right kind of object
    class(accessors) <- 'cacheMatrix'
    
    # return the vector
    accessors
}

# DESCRIPTION:
# Computes the invers of the matrix stored in the parameter 'x'.
# The object passed to 'x' needs to be created with 'makeCacheMatrix' 
#
# EXAMPLE:
# > c <- makeCacheMatrix(matrix(c(1,2,3,5), 2, 2))
# > cacheSolve(c)
#      [,1] [,2]
# [1,]   -5    3
# [2,]    2   -1
cacheSolve <- function(x, ...) {
    # check preconditions
    stopifnot(class(x) == 'cacheMatrix')
    
    # get the invers from cache
    inv <- x$getinv()
    
    # if the invers wasn't cached, compute it and store it in cache
    if(is.null(inv)) {
        mat <- x$getmat()
        inv <- solve(mat, ...)
        x$setinv(inv)
    }
    
    # return the invers
    inv
}

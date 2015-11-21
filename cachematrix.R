# Below are two functions from the assignment allowing for
# creation of a matrix and computing and caching the inverse of the matrix.
# Please also find an alternative solution at the bottom - makeAutoCacheMatrix

# Creates a list containing methods for accessing a matrix 
# and its computed inverse
#
# Args:
#   x: Square matrix containing initial data.
#      If no value passed, an empty (0,0) matrix is assigned
#
# Returns:
#   A list with methods get, set, getInverse, setInverse
#   for accessing and changing the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    getInverse <- function() inverse
    setInverse <- function(i) inverse <<- i
    
    list(
        get = get, 
        set = set, 
        getInverse = getInverse, 
        setInverse = setInverse)
}


# Computes an inverse of a matrix.
# If the inverse is not set on the cacheMatrix - computes and caches the value.
# If it was computed earlier it returns the cached value.
#
# Args:
#   x: 'cacheMatrix' list created by makeCacheMatrix
#
# Returns:
#   An inverse of the matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
}


# Another approach to storing a cached inverse for a matrix.
# This solution has some desirable features, such as:
# immutability - underlying matrix can not be changed
# encapsulation - hides implementation details from the user
# conciseness - all functionality is wrapped in a single object
#       i.e. there's no need for two functions in the environment
# Sample usage:
# > m <- makeAutoCacheMatrix(matrix(c(4, 1, 3, 1), 2, 2))
# > i <- m$getInverse()
# solving for inverse
# > i
# [,1] [,2]
# [1,]    1   -3
# [2,]   -1    4
# > i <- m$getInverse()
# > i
# [,1] [,2]
# [1,]    1   -3
# [2,]   -1    4
makeAutoCacheMatrix <- function(m = matrix()) {
    inverse <- NULL
    get <- function() m
    getInverse <- function() {
        if (!is.null(inverse)) return(inverse)
        message("solving for inverse")
        inverse <<- solve(m)
        return(inverse)
    }
    
    list(get = get, getInverse = getInverse)
}

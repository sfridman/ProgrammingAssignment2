# Peer Exercise 2 - R Course 

# Inverse a matrix efficiently - with ability to cache its inverse without recalculating it
# makeCacheMatrix - Builds the matrix access for the inverse on need only
# cacheSolve - Use the inverse efficiently - build if necessary, take from cache if exists 

# Build a matrix API supporting a cached inversed matrix 
# Input: x = Given matrix - assumed to be a matrix and invertible 
# Output: List of 4 accessor functions - get/set for matrix, and get/set for its inverse 
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    # Matrix accessors 
    setMatrix <- function(mat) {
      x <<- mat
      inverse <<- NULL
    }
    getMatrix <- function() x

    # Inverse accessors 
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

# Inverse a matrix efficently using cache if such exists
# Input: x = An invertible matrix 
# Output: The inverse of x 
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()

    # Cahced value exists - use it 
    if(!is.null(inv)) {
        message("Getting inverse from cached data")
        return(inv)
    }

    # No cahced value exists - "compute" and cache for future 
    mat <- x$getMatrix()
    inv <- solve(mat)
    x$setInverse(inv)
    inv
}

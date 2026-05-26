makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse property
        inv <- NULL
        
        ## Method to set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Method to get the matrix
        get <- function() x
        
        ## Method to set the inverse of the matrix
        setInverse <- function(inverse) inv <<- inverse
        
        ## Method to get the inverse of the matrix
        getInverse <- function() inv
        
        ## Return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        ## Just return the inverse if it is already set
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## Get the matrix from our object
        mat <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        inv <- solve(mat, ...)
        
        ## Set the inverse to the object
        x$setInverse(inv)
        
        ## Return the matrix
        inv
}

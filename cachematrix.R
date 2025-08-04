## his function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
        # Initialize the inverse as NULL
        inv <- NULL
        
        # Set function to assign a new matrix and reset the inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Get function to retrieve the matrix
        get <- function() x
        
        # Set function to cache the inverse
        setinverse <- function(inverse) inv <<- inverse
        
        # Get function to retrieve the cached inverse
        getinverse <- function() inv
        
        # Return a list of the functions
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        # Check if the inverse is already cached
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # If not cached, compute the inverse
        data <- x$get()
        inv <- solve(data, ...)
        
        # Cache the inverse
        x$setinverse(inv)
        
        # Return the inverse
        inv
}

# Similar to the function given in the assignment we make the inverse cache matrix functions

makeCacheMatrix <- function( m = matrix() ) {

        i <- NULL

        # Set the matrix
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }

        # Get the matrix
        get <- function() {
                # Return the matrix
                m
        }

        setInverse <- function(inverse) {
                i <<- inverse
        }

        getInverse <- function() {
                i
        }

        # List of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


cacheSolve <- function(x, ...) {

        # Return inverse of 'x'
        m <- x$getInverse()

        # Return inverse if it's already set
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }

        # Get matrix from object
        data <- x$get()

        # Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data

        # Set the inverse to the object
        x$setInverse(m)

        # Return the matrix
        m
}

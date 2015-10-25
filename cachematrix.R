## Since this function will be relatively similar to the given example function
## the descriptions can also be pretty similar.

## This function creates a "special" matrix object that can cache the inverse
## of a matrix. It is really a list containing the functions to 
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse matrix
##      4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This following function takes the "special" matrix created with the function 
## above and checks if the inverse matrix is already stored in the cache. If it 
## it is not, it will calculate the inverse matrix and store it in the cache.
## Either way it returns a matrix that is the inverse of given matrix.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
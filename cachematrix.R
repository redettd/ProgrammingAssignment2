## Below are two functions that are used to create a special object
## that stores a numeric matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to

##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse of the matrix
##  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        x_inverse <- NULL
        set <- function(y) {
                x <<- y
                x_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) x_inverse <<- inverse
        getinverse <- function() x_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the setinverse
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setinverse(inverse)
        inverse
}

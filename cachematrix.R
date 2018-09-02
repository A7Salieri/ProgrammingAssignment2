## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  The following function creates a special matrix, which is really a list containing a function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) { ##
            x <<- y
            m <<- NULL
    }
    get <- function() x ##returns x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## The following function calculates the inverse of the special matrix created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()     ## read in inverse
        if(!is.null(m)){        ## if inverse is already cached, say so and return it.
                message("Getting cached data.")
                return(m)
        }
        data <- x$get() 
        m <- solve(data, ...) ## otherwise, solve for the inverse.
        x$setinverse(m)       ## set inverse in cache
        m                     ## output m
}

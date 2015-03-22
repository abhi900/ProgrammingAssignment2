## Put comments here that give an overall description of what your
## functions do :- Cache the inverse of a matrix.

## This function creates a type of a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 ## Initializing the inverse property
        inv <- NULL
        
        ## Method to set the matrix
        set <- function(y){
                matrix <<- y
                inv <<- NULL
        }
        
        ## Method to get the matrix
        get <- function(){
                ## returning matrix
                matrix
        }
        
        ## Method to set the inverse of the matrix
        setInverse <- function(inverse) {
                ## storing inverse 
                inv <<- inverse
        }
        
        ## Method to get the inverse of the matrix
        getInverse <- function() {
                ## returns the inverse
                inv
        }
        
        ## Returns the list of methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the matrix as returned by the "makeCacheMatrix" function from above. 
## If the inverse has already been calculated (and the matrix has not changed), then the purpose of "cachesolve" is to retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## getting a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        ## returns if the inverse has already been calculated (i.e. if !is.null(m)==TRUE)
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        
        ## If the inverse was not calculated before and cached then
        
        ## getting the matrix from our object created
        data <- x$get()
        
        ## calculating the inverse by using matrix multiplication
         m <- solve(data) %*% data
        
        ## storing the inverse of an object for future usage
        x$setInverse(m)
        
        ## returns a matrix that is the inverse of 'x'
        m 
}

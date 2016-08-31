## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        cached_inverse <- NULL
    
        set <- function(y) {
            x <<- y
            cached_inverse <<- NULL
        }
    
        get <- function() {
            x
        }
    
        setinverse <- function(inverse) {
            cached_inverse <<- inverse
        }
    
        getinverse <- function() {
            cached_inverse
        }
    
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        if(!is.null(inverse)) {
            message("Getting cached data.")
            return(inverse)
        }
        
        data <- x$get()
        
        inverse <- solve(data, ...) 
        
        x$setinverse(inverse)
        
        inverse
}


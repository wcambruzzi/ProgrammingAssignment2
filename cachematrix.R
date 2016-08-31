## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        cached_inverse <- NULL ##Create variable for cached inverse matrix
    
        set <- function(y) { ##This function set a new matrix and clear the inversed cached matrix
            x <<- y
            cached_inverse <<- NULL
        }
    
        get <- function() {  ##Return the matrix
            x
        }
    
        setinverse <- function(inverse) { ##Set the cached inversed matrix
            cached_inverse <<- inverse
        }
    
        getinverse <- function() { ##Get the inverved cached matrix
            cached_inverse
        }
    
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse() ##Get de inversed cached matrix
        
        if(!is.null(inverse)) { ##Test if the inverse cached is null
            message("Getting cached data.")
            return(inverse) ##if not is null return de inversed cached matrix
        }
        
        data <- x$get()  ##Get de matrix
        
        inverse <- solve(data, ...)  ##Inverse de matrix
        
        x$setinverse(inverse) ##Set inversed matrix with setinverse
        
        inverse ##Return a inverse matrix
}


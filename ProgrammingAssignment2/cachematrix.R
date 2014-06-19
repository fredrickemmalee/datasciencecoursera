## The first function makes a special "matrix" that, once calculated, 
## will store/cache the value of its inverse. As calculating a 
## matrix inverse can be time intensive this may be advantageous. 
## The second function computes the inverse given one of these
## special matrices. 

## This function uses lexical scoping ideas to cache the inverse 
## of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
    
    inv = NULL
    
    set <- function(y) {
        ## the <<- operator is used to assign a value to an object 
        ## in an environment that is different from the current one.
        x <<- y 
        inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inverse) inv <<- inverse
    
    getinv <- function() inv
    
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)
}


## This function obtains or solves for the inverse of one of 
## the special matrices given above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    if(!is.null(inv)){
        message("getting cache data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    
    inv
}

## Macro View:  makeCacheMatrix function creates a special "matrix" object
## cachesolve function computes the inverse of this "matrix object.
## In addition, if the inverse has already been calculated, then cachsolve function will retrieve this inverse from the cache.
## Input: e.g. a <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2)), a$get(), a$getinvmatrix(), cachesolve(a).

makeCacheMatrix <- function(x = matrix()) {
        invmatrix <- NULL
        set <- function(y) {
                x <<- y
                invmatrix <<- NULL
        }
        get <- function() {x}
        setinvmatrix <- function(inverse) {invmatrix <<- inverse}
        getinvmatrix <- function() {invmatrix}
        list(set = set, get =get,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
}

## cachesolve function
## calls makeCacheMatrix to see if inverse has already been stored in cache.
## Message if it is already stored(not NULL) and retrieves stored inverse.

cachesolve <- function(x, ...) { 
        
        invmatrix <- x$getinvmatrix()               
        if(!is.null(invmatrix)) {               
                
                message("getting cached data")  
                return(invmatrix)                       
                
        }
        ## Calculate the inverse matrix as part of cachesolve.
        ## Only reach this if x$getinvmarix is NULL.

        data <- x$get()                 
        invmatrix <- solve(data, ...)            
        
        x$setinvmatrix(invmatrix)                    
        invmatrix                                 
}   

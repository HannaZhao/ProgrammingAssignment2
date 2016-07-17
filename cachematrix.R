## 

## This function creates a special "matrix" object. It returns a list containing four functions. 
##1. set the value of matrix.
##2. get the value of matrix.
##3. set the value of inverse matrix.
##4. get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
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


## This funciton computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
##should retrieve the inverse from the cache. Otherwise, it will calculate the inverse and solve it 
## in the cache by inv.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        maxt <- x$get()
        inv <- solve(maxt, ...)
        x$setinv(inv)
        inv
}

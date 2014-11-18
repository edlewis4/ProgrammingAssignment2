## The program will create a matrix and compute the inverse of the matrix. 
## It will read from cache if the inverse has been previously computed.

## This function creates a special "matix" object than can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL   ## reset matrix to NULL 
        
        set <- function(y) {
                x <<- y                           ## save input matrix
                m <<- NULL                        ## reset inverse matrix to NULL
        }
        
        get <- function() x                        ## Return value of original matrix
        setinverse <- function(solve) m <<- solve  ## calculate inverse
        getinverse <- function() m                 ## return cached value if exist
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matix" returned
## by function makeCacheMatrix above.  If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve will retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()   ## access object x and get Inversematrix
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)     ## return the inverted cached matrix
        }
        data <-x$get()        ## No prior cache exits.  Get matrix to invert
        m <-solve(data, ...)  ## calculate inverse of matrix
        x$setinverse(m)       ## store inverse matrix in matrix object
        m                     ## return the inverted matrix
}

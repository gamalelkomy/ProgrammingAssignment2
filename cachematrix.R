## The following code is to compute the inverse of a square matrix


## The following function is to make the cache for the matrix and
## the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## creating the function set to set the value in X
        set <- function(y) {
                
                x <<- y
                m <<- NULL
        }
        ##creating the function get to get the value of x
        get <- function() x
        #creating the function setInverse to set the inverse in m
        setInverse <- function(inverse) m <<- inverse
        ##creating the function getInverse to get the inverse from m
        getInverse <- function() m
        
        ## to return the list 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function is to compute the inverse of the matrix
## if it does not exist in the cache

cacheSolve <- function(x, ...) {
        
        m <- x$getInverse()
        
        ## if the inverse is already computed return it and end
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if the inverse does not exist
        ## get the value
        data <- x$get()
        ##calculate the inverse
        m <- solve(data, ...)
        ##set the inverse inside the vector
        x$setInverse(m)
        
        ## return the inverse
        m
}

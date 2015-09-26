## 4 functions 
## set == to set the matrix initialization
## get == to return the matrix as it is
## setinverse == to calculate the inverse of the matrix
## getinverse == to return the inverse cached by the setinverse

## Returns list of functions to set and get cache of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## initialize the inverse to null
        i <- NULL
        ## first set the matrix and initialise inverse to NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## get the matrix as it is
        get <- function() x
        ## cache the matrix inverse into i
        setinverse <- function(x) i <<- x
        ## return the inverse 
        getinverse <- function() i
        ## Return the list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#### Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
        ## i is going to be the return value, so lets initialize it
        i <- matrix()
        ## check if we can get the inverse from the cache first
        i <- p$getinverse()
        ## if the cache is available, then return i
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## else if cache is not available, then use set/get function to cache/fetch the matrix
        p$set(x)
        data <- p$get()
        ## get the inverse into i
        i <- solve(data)
        ## set the inverse in cache
        p$setinverse(i)
        ## return i
        i
}
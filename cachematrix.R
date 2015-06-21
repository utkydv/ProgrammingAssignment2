## The pair of functions written below help in catching the inverse of a matrix.
## The functions cache the inverse & avoid re-calculation if the same is already 
## available from a previous calculation.


## This function creates a special matrix with a set of functions. This function
## also enables catching of the inverse of this special matrix   
makeCacheMatrix <- function(x = matrix()) {
        xinverse <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) xinverse <<- inv
        getinverse <- function() xinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 

}


## This function checks if the inverse value of the matrix is already available (stored through previous function)
## & retrieves it. Else, calculates the matrix inverse.
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i  
}

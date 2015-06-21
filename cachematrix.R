## The pair of functions written below help in catching the inverse of a matrix.
## The functions cache the inverse & avoid re-calculation if the same is already 
## available from a previous calculation.


## This function creates a special matrix with a set of functions. This function
## also enables catching of the inverse of this special matrix   
makeCacheMatrix <- function(x = matrix()) {
        ## sets vairable holding the inverse of 'x' to NULL
        xinverse <- NULL
        ## To set the values inside the matrix 'x'
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## To retrieve the matrix in context here
        get <- function() x
        ## setting the inverse value to be catched
        setinverse <- function(inv) xinverse <<- inv
        ## retrieve the catched inverse value
        getinverse <- function() xinverse
        ## returns the list coantaining the special functions for the matrix created through this function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 

}


## This function checks if the inverse value of the matrix is already available (stored through previous function)
## & retrieves it. Else, calculates the matrix inverse.
cacheSolve <- function(x) {
        ## Checks if the inverse value already exists
        i <- x$getinverse()
        ## displays the right message & returns the catched inverse matrix
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## In case there is no pre-calculated value, the original matrix is retrieved
        data <- x$get()
        ## Calculate the inverse value using 'solve' function
        i <- solve(data)
        ## stores the calcutated inverse value for future use
        x$setinverse(i)
        ## Return a matrix that is the inverse of 'x'
        i  
}

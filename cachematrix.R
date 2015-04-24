## This code describes a structure that stores a matrix and can store an inversed matrix.
## If you need to get your matrix inversed use cacheSolve function. It will return cached value of inversed matrix if it is already calculated.

## Structure that contain original matrix and can contain inversed matrix, if already calculated
## x - original matrix
## invm - inversed matrix, which is NULL when x just assigned and inversed matrix is not 
## calculated yet, and inversed matrix if it is already calculated
makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinversed <- function(inversed) invm <<- inversed
        getinversed <- function() invm
        list(set = set, get = get,
             setinversed = setinversed,
             getinversed = getinversed)
}


## This function calculates and sets inversed matrix for structure above, if inversed matrix 
## was not calculated yet, or just gets inversed matrix from the structure if it is already calculated
cacheSolve <- function(x, ...) {
        m <- x$getinversed()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) ##
        x$setinversed(m)
        m
}

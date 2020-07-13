## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## initialising x and inverse function assigned as inv
        set <- function(y) { ## define behaviours, assumes y as a numeric vector
                x <<- y ## assigns value y to x in parent environment
                inv <<- NULL ## this code clears any value of inv cached in memory of object and forcing to recalculate inverse
        } ## assumes y as a numeric vector
        get <- function() {x} ## a lexical scoping feature retrieving x from parent environment
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## creates a new object by returning a list()
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {## return a matrix that is the inverse of 'x'
        inv <- x$getInverse() ## gets inverse of x
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv
}

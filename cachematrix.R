## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	r <- NULL
        set <- function(y) {
                x <<- y
                r <<- NULL
        }
        get <- function() x
        setreverse <- function(reverse) r <<- reverse
        getreverse <- function() r
        list(set = set, get = get,
             setreverse = setreverse,
             getreverse = getreverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         r <- x$getreverse()
        if(!is.null(r)) {
                message("getting reversed matrix")
                return(r)
        }
        data <- x$get()
        r <- solve(data, ...)
        x$setreverse(r)
        r
}

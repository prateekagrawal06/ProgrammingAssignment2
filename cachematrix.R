## Two function written bwlow are used to stored the value of 
##reverse ofa matriz in different environment so that computation can be achieved faster for large files

## This function created a special vector of function that
##sets and gets the reverse value of a matrix

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


## Check if reverse of the matrix is available in cache. If it is then the same value is returned and no computation is made
## else the value of reverse os calculated and stored in the cache memory

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

## Create "cache-able matrix" for solving inverse of matrices

## create the actual new matrix type

makeCacheMatrix <- function(x = matrix()) {
    m = NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix = function(solve) m <<- solve
    getmatrix = function() m
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)

}


## Check to see if inv is cached and return avail data. Otherwise, compute the inv and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m = x$getmatrix()
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    data = x$get()
    m = solve(data, ...)
    x$setmatrix(m)
    m
}

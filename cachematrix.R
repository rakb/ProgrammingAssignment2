## Programming Assignment 2 : Caching the Inverse of a Matrix
## The two functions below are used to cache the inverse of a matrix.
## If inverse of the same matrix is repeatedly invoked then value is fetched from
## the cache instead of re-computing it. This saves the time to perform inverse.


## The makeCacheMatrix is like a class that encapsulates both the
## methods/functions to access the data and also the data itself.
## The data here is the inverse of a given matrix.
## set/get functions are provided to access the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(inv) m <<- inv
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## The cacheSolve function is responsible for computing the inverse of the matrix.
## It uses the solve() function to do so. Before invoking the solve() function, it
## checks if the inverse of the matrix is already computed, if so it returns the same.

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
		## return inverse of matrix
        m
}

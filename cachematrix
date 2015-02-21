## Put comments here that give an overall description of what your
## functions do

## The functions will create an inverse matrix and then cache the inversed matrix in another "namespace" or environment. Then if the inverse is required later the function below can retreive the cached inverse matrix, thus saving time and making the process more effecient.



## Write a short comment describing this function

## This function will take the arguement and cache the inverse
makeCacheMatrix <- function(x = matrix()) {
    
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)

}


## Write a short comment describing this function

## This function will first check to see if an inverse was cached, and if so it will return the cached inverse. If the inverse wasn't cached then the function will create the inverse and cache it for later use.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}

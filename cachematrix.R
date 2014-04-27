## The following pair of functions will allow to 
## 1) retrieve the cached inversed matrix if it has been pre-computed
## 2) cache the inversed matrix if it has NOT been pre-computed
## usage (assuming mm is an inversible matrix)
## x <- makeCacheMatrix(mm)
## cacheSolve(x)

## this function creates an object that can be used to cache the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# the first time call of the following function so to solve the inverse matrix 
# the subsequent calls will retrive the cached inversed matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

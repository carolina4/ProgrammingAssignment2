## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    
    message("calculating inverse from data")
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    
    i
}

run <- function() {
    m <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
    print(m)
    y <- makeCacheMatrix(m)
    
    res <- cacheSolve(y)
    print(res)
    
    res <- cacheSolve(y)
    print(res)
}
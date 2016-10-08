##This function creates and stores a "matrix" object that can cache 
##the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function executes or computates the inverse function generated
##by the "makeCacheMatrix" function above. If the function has been
##calculated then it should get the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Data cached")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

##Below is some FYI testing for the code above. 
> getm <- makeCacheMatrix(matrix(1:4, 2, 2))
> getm$get()
[,1] [,2]
[1,]    1    3
[2,]    2    4
> getm$getInverse()
NULL
> getm <- makeCacheMatrix(x = matrix(1:4,2,2))
> getm$getInverse
function() inv
<environment: 0x0000000005a4f460>
        > getm$getInverse()
NULL
> getm$get
function() x
<environment: 0x0000000005a4f460>
        > getm <- makeCacheMatrix(matrix(1:4,2,2))
> getm$get()
[,1] [,2]
[1,]    1    3
[2,]    2    4
> getm$getInverse()
NULL
> cacheSolve(getm)
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(getm)
Data cached
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
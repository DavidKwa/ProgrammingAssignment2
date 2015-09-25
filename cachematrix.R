# This script has 2 functions:
# 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#               If the inverse has already been calculated (and the matrix has not changed),
#               then the cachesolve should retrieve the inverse from the cache.



# This function stores a martix and a cached value of the inverse of the matrix. 
# It returns a list of the following 4 functions:
# 1. setmtx     set the value of a matrix
# 2. getmtx     get the value of a matrix
# 3. setinv     set the cached value of the inverse of the matrix
# 4. getinv     get the cached value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setmtx <- function(y) {
        x <<- y
        m <<- NULL
    }
    getmtx <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(setmtx = setmtx,
         getmtx = getmtx,
         setinv = setinv,
         getinv = getinv)
}



# This following function calculates the inverse of a "special" matrix created with the makeCacheMatrix function
cacheSolve <- function(x, ...) {
    #get the cached value of the inverse of matrix
    m <- x$getinv()
    #if the cached value is NOT null, return the inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #if the cached value IS null, get the matrix
    data <- x$getmtx()
    #use solve function to calculate the inverse of the matrix
    m <- solve(data, ...)
    #store the calculated inverse in the cache
    x$setinv(m)
    #return the inverse
    m
}

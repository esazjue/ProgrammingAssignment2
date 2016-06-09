## This pair of functions make possible to work with a new matrix-like "object" that 
## besides the matrix itself it is able to store its inverse matrix
## Function makeCacheMatrix() creates the object and cacheSolve() calculates its inverse
## in case if hasn't been calculated before and stores the restult in the matrix object 
## passed as an argument. If the inverse matrix already exists, it simplies return the ## value avoiding having to calculate it again


## This function returns a list of functions that makes possible to handle the "cached 
## matrix" object. The list has four elements which are basically the set/get pair of 
## functions to handle the original matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setmatrixinverse <- function(matrixinverse) inv <<- matrixinverse
    getmatrixinverse <- function() inv
    list(set = set, get = get,
         setmatrixinverse = setmatrixinverse,
         getmatrixinverse = getmatrixinverse)

}


## This function takes a "cached matrix" object and return its inverse matrix. If the 
## inverse matrix was already calculated, it simply returns it. If that is not the case,
## the inverse matrix is calculated and its value is first stored in the "cached matrix ## and the returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getmatrixinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrixinverse(m)
    m
}

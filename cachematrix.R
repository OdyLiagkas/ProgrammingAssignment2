## The makeCacheMatrix function creates a list containing 4 sub functions.
## set, sets the value of the matrix
## get, returns the value of the matrix
## setinv, sets the value of the inverse of the matrix
## getinv, returns the value of the inverse of the matrix

## The makeCacheMatrix function creates a list containing 4 sub functions.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x<<-y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inversion){
        i <<- solve(inversion)
    }
    getinv <- function() i
    l <<- list(set=set, get=get, setinv=setinv, getinv=getinv)
    return(l)
}


## The cacheSolve function checks to see if i is NULL, meaning that the inverse 
## of the matrix has not been calculated, and if it has, it returns its value
## while if it hasn't it calculates it through the makeCacheMatrix
cacheSolve <- function(l=l, ...) {
    i <- l$getinv()
    if(!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }
    data <- l$get()
    i <- solve(data, ...)
    l$setinv(i)
    i
}
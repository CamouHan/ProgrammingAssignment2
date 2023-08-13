## In overall, the first function creates a list containing four functions and 
## keeps the inverse of the matrix if it was calculated to prevent extra calculations
## And the second function calculates the inverse of the matrix. However, it checks 
## at first if it was calculated already and if it was, it returns the result which is
## ready already and if it was not stored, it calculates the value and store it in the 
## first function

## This function creates a vector which is actually a list and this list 
## contains functions like set get or setting inverse or getting inverse of a matrix argument 

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<-y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() i
    list(set=set,get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function calculates the inverse of a matrix. However it checks if it was
## calculated at first or not. Accordingly, it gives the already calculated value
## or calculate it.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setinverse(inverse)
    inverse
    
}

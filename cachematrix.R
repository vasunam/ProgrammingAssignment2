## This function creates a special "matrix" object that can cache 
## its inverse.

## expected input: matrix object
## expected output: list

makeCacheMatrix <- function(X = matrix()) {
    inv <- NULL
    # if matrix is not set, set a matrix
    set <- function(y){
        X <<- y
        inv <<- NULL
    }
    
    # getting matrix
    get <- function() X
    
    # setting and getting inverse of the matrix
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    
    list(set=set, get=get,
         setinv=setinv, 
         getinv=getinv)
}



## expected input: list object
## expected output: matrix that is the inverse of 'x'

cacheSolve <- function(X, ...) {
    
    # geting invers
    inv <- X$getinv()
    
    # checking if invers is done 
    if(!is.null(inv)){
        message("getting cached matrix")
        return(inv)
    }
    
    # getting matrix to the data
    data <- X$get()
    # inverting matrix
    inv <- solve(data, ...)
    # "saving" inverted matrix
    X$setinv(inv)
    # printing inverted matrix to the screen
    print(inv)    
}
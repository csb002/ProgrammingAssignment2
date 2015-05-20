## These are a pair of functions that cache
## the inverse of a matrix.

## Function creates a list of functions
## which will be used to store the inverse.

makeCacheMatrix <- function(x = matrix()) {
        #Matrix to store inverse matrix
        inv <- NULL 
        #Function-substitutes matrix x with matrix y in main function
        set <- function(y){
                x <<- y
                #sets old inverse matrix to null
                inv <<- NULL
        }
        #Function-Retrieves the matrix given by main function
        get <- function() x
        #Function-stores value of the inverse into the main function
        setinverse <- function(inverse) inv <<- inverse
        #Function-Retrieves inverse matrix
        getinverse <- function() inv
        #List of the 4 functions
        list(set = set, get = get, 
              setinverse = setinverse,
              getinverse = getinverse)
}


## Function computes the inverse of the matrix 
## formed by makeCacheMatrix. First checks if 
## the inverse has already been computed and 
## computes inverse if it has not. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() 
        #checking if inverse is already computed
        if(!is.null(inv)){
              message("getting cached data")
              return(inv)
        }
        #if no inverse, gets matrix and computes inverse
        data <- x$get()
        inv <- solve(data,...)
        #stores inverse in makeCacheMatrix
        x$setinverse(inv)
        inv
}

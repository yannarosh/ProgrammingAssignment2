## The function makeCacheMatrix creates a matrix object and can cache its inverse.

makeCacheMatrix <- function(X = matrix()) {
        # initialize inverse matrix I as null.
        I <- NULL
        
        # set X to whatever is passed as the argument and initialize inverse matrix I as null.
        set <- function(y) {
                X <<- y
                I <<- NULL
        }
        
        # return the input matrix X.
        get <- function() {
                X
        }
        
        # set value of inverse matrix I to whatever is passed as the argument.
        setinverse <- function(inv){
                I <<- inv
        }
        
        # return the inverse matrix I.
        getinverse <- function() {
                I
        }
        
        # create a list of functions.
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the matrix created 
## with the above function. It first checks to see if the inverse I has already been calculated. 
## If so, it gets I from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache 
## via the setmean function.

cacheSolve <- function(X, ...) {
        
        # the function looks for I in the cache and if there's something there, it prints a message
        I <- X$getinverse()
        if (!is.null(I)) {
                message("getting cached data...")
                I
        }
        
        # get the matrix to be inverted from the cache and solve it.
        data <- X$get()
        I <- solve(data, ...)
        
        # store the inverted to the cache and print it
        X$setinverse(I)
        I
}

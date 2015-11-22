##Matrix inversion is usually a costly computation and 
##there may be some benefit to caching the inverse of a matrix
##rather than computing it repeatedly.
##These two functions cache and compute the inverse of a matrix.


##The function makeCacheMatrix() creates a special "matrix" object that can cache its inverse.
## It returns a list containing functions to:
##  -set the matrix
##  -get the matrix
##  -set the inverse
##  -get the inverse
## The list returned is then used as the input to cacheSolve() function below.
## The function also make use of <<- operator which is used to assign a value to an object in an environment 
## different from the current environment. 

makeCacheMatrix <- function(x = matrix()) {
  
    i= NULL
    set = function(y) {
    
        x <<- y
        i <<- NULL
  }
  
    get = function() x
    setinverse = function(inverse) i <<- inverse 
    getinverse = function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
 

## The cacheSolve() function calculates the Inverse of the special "Matrix" object
## created with the above function makeCacheMatrix(). It first checks to see if the inverse
## has already been calculated. If it has, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the data and 
## sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
    i <- x$getinverse()
    
        if(!is.null(i)) {
          message("getting cached data")
        return(i)
    }
      
      data <- x$get()
      i <- solve(data, ...)
  
      x$setinverse(i)
      
      i
}

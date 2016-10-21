## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix and stores a matrix.
##this functions sets a list of functions that have set values

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The function below is where the inverse is actually calculated.
##it first asks to get the inverse, and if it is not null then it has to retrieve the inv that was stored
##if it's null then it will calculates it and stores it in the previous function
cacheSolve <- function(x,...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

    
 ##example   
 x = rbind(c(1, -1.6), c(-1.9, 1))
 m = makeCacheMatrix(x)
 m$get()
    
cacheSolve(m)    
    

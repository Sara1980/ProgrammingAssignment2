## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL              #initialize inv as NULL; will hold value of matrix inverse
  setMatrix <- function(y) {     #define function to set the value of the Matrix 
    x <<- y 
    invMatrix <<- NULL           #if there is new matrix, reset inv to NULL
  } 
  getMatrix <- function() x                              #defines the get function - returnes value of the matrix
  setInverse <- function(inverse) invMatrix <<- inverse  #set the value of the invertible matrix 
  getInverse <- function() invMatrix                     #get the value of the invertible matrix where called
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() 
  if(!is.null(inv)) { 
    message("getting cached data") 
    return(inv) 
  } 
  data <- x$get() 
  inv <- solve(data, ...) 
  x$setinverse(inv) 
  inv 
}

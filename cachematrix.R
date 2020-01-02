## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) { 
  #initialize inv as NULL; will hold value of matrix inverse
  invMatrix <- NULL 
  #define function to set the value of the Matrix 
  setMatrix <- function(y) { 
    x <<- y 
    #if there is new matrix, reset inv to NULL
    invMatrix <<- NULL 
  } 
  getMatrix <- function() x                              #defines the get function - returnes value of the matrix
  setInverse <- function(inverse) invMatrix <<- inverse  #set the value of the invertible matrix 
  getInverse <- function() invMatrix                     #get the value of the invertible matrix where called
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse) 
} 

cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x' 
  inv <- x$getinverse()      
  if(!is.null(inv)) {                 #if inverse matrix is not NULL
    message("getting cached data")     #Type message: Getting Cached Invertible Matrix 
    return(inv)                        #return the invertible matrix
  } 
  data <- x$get()                   #get the original Matrix Data 
  inv <- solve(data, ...)           #use solve function to inverse the matrix
  x$setinverse(inv)                  #set the invertible matrix 
  inv                                #return the invertible matrix
} 
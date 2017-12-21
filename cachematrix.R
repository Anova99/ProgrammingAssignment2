## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The function "makeCacheMatrix" takes an invertible matrix as argument and returns 
#a list of functions that will be used by the "cacheSolve" function to calculate the  
#inverse of that matrix and store its result.
#The function "set" sets the argument matrix
#The function "get" gets the argument matrix
#The function "setInverse" sets the inverse matrix of the argumnent matrix
#The function "getInverse" gets the inverse matrix of the argument matrix


rm(list=ls())

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(Y) {
    x <<- Y
    Inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) Inv <<- Inverse
  getInverse <- function() Inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
#The function "cacheSolve" either caculates the inverse matrix of the argument matrix "x"
#or returns its value if it has already been calculated before.

cacheSolve <- function(x, ...) {

  Inv <- x$getInverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInverse(Inv)
  Inv
  }

#Test

M <- matrix(c(1,1,2,1),nrow = 2)
MM <- makeCacheMatrix(M)
cacheSolve(MM)
M%*%cacheSolve(MM) #The product must be the identity matrix of dimension 2

#here we reset the argument matrix

N <- matrix(c(1,0,1,1),nrow = 2)
MM$set(N)
cacheSolve(MM)
N%*%cacheSolve(MM) #The product must be the identity matrix of dimension 2

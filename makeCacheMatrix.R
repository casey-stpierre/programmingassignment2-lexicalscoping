## In this project I am taking an object (in this case, a 2x2 matrix) and 
## finding its inverse. As this is a time consuming computation, I will 
## be caching these for futher time saving use. 

## Here I am making an object in the makeCacheMatrix function that will
## eventually cache the inverse of it.
makeCacheMatrix <- function(x = matrix()){
      inv <- NULL
      set <- function(y){
        x <<- y
        inv <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The cacheSolve function will take what is returned from makeCacheMatrix
## and determine if it has calculated the inverse of the matrix correctly.
## If it has correctly returned the inverse, then it will retrieve it the 
## cached inverse.
cacheSolve <- function(x, ...){
      inv <- x$getInverse()
      if(!is.null(inv)){
          message("getting cached data")
          return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}


source("makeCacheMatrix.R")
pmatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
pmatrix$get()
pmatrix$getInverse()
cacheSolve(pmatrix)
cacheSolve(pmatrix)
pmatrix$getInverse()

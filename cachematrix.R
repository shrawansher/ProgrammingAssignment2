## The makeCacheMatrix creates a list which contains the following functions 
## as their elements - getMatrix,setMatrix(y),getInverse,setInverse(inv)

## getMatrix()  - fetches the matrix
## setMatrix(y) - sets the value of the matrix in the environment
## getInverse() - fetches the inverse of the matrix
## setInverse() - sets the inverse 

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  setMatrix <- function(y){
    x <<- y
    invmatrix <<- NULL
  }
  getMatrix <- function() x
  getInverse <- function() invmatrix
  setInverse <- function(inv) {
    invmatrix <<- inv
  }
  list(setMatrix = setMatrix, getMatrix = getMatrix,getInverse = getInverse, setInverse = setInverse)

}


## The function fetches the inverse matrix from makeCacheMatrix and checks the null condition for it
## If the matrix is null it assigns it the the inverse and sets the Inverse matrix to the env
## Else it fetches the cached value through the getInverse()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmatrix <- x$getInverse()
  if(!is.null(invmatrix)){
    message("getting cached data")
    return(invmatrix)
  }
  else{
    data <- x$getMatrix()
    inv <- (solve(data))
    x$setInverse(inv)
    inv
  }
}

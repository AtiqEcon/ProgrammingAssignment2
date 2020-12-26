
###########################  Codes  start here #################################

## Creating a matrix object, named makeCacheMatrix that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv_of_x <- NULL
  set <- function(y){
    x<<- y
    inv_of_x <<- NULL
  }
  
  
  
  ## Getting the matrix using following method: 
  
  get <- function(){x}
  
  ## Setting the inverse of the matrix using following method:
  
  setInverse <- function(inverse) {inv_of_x <<- inverse}
  
  ## Getting the inverse of the matrix using following method:
  
  getInverse <- function() {inv_of_x}
  
  ## Creating a list of the methods to return using the following codes:
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Cache is an important function in R that computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  inv_of_x <- x$getInverse()   ## Return a matrix that is the inverse of 'x'
  if(!is.null(inv_of_x)){
    message("getting cached data")
    return(inv_of_x)
  }
  mat <- x$get()
  inv_of_x <- solve(mat, ...)
  x$setInverse(inv_of_x)
  inv_of_x
}

########################### Result #################################


matrix_of_x <- makeCacheMatrix(matrix(1:4, 2, 2))

matrix_of_x$get()

matrix_of_x$getInverse()

cacheSolve(matrix_of_x)
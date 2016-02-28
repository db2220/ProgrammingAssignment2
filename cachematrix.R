## Matrix inversion can be an expensive computation so let's cache the inverse of a 
## matrix rather than compute it repeatedly 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # instantiate the matrix 
  myMatrix <- NULL
  
  # set the value of the matrix
  set <- function(y){
    x <<- y
    myMatrix <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse matrix
  setInverseMatrix <- function(solve) myMatrix <<- solve
  
  # get the value of the matrix
  getMatrix <- function() myMatrix
  
  # return the list of functions
  list(set = set, get = get, setInverseMatrix = setInverseMatrix,
       getMatrix = getMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # get the inverse of the matrix
  myMatrix <- x$getMatrix
  
  # if the inverse of the matrix has already been calculated, pull from cache
  if(is.null(myMatrix)) {
    message("using cached data")
    return(myMatrix)
  }
  
  # Otherwise, calculate the inverse of the matrix, cache and return it
  data <- x$get()
  myMatrix <- solve(data, ...)
  x$setInverseMatrix(myMatrix)
  myMatrix
  
}

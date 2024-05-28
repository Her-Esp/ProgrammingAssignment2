## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ##Initialize the inverse property
  inv <-NULL
  
  ## Method to set the matrix
  set <- function(matrix){
      matr <<- matrix
      inv <<- NULL
  }
  
  ## Method to get the matrix
  get <- function() matr
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  ##Method to get the inverse of the Matrix
  getInverse <- function() inv
  
  ##Returns a list of the methods
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of "x"
  matr <- x$getInverse()
  
  ## Returns the inverse if its already set
  if( !is.null(matr) ) {
      message("getting cached data")
      return(matr)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate thee inverse using matrix multiplication
  matr <- solve(data) %*% data
  
  ## Set the inverse of the object
  x$setInverse(matr)
  
  ## Return the matrix
  matr
}

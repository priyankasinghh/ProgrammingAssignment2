## Put comments here that give an overall description of what your
## functions do

## Function which stores inverse of the input matrix.
## It doesn't compute inverse, but once inverse is set. It remembers it unless input matrix is changed via 'set(...) sub method.
## It provides 4 sub methods
##    1. set(y) : Resets the matrix with y and inverse with NULL.
##    2. get() : Returns the matrix.
##    3. get_inverse() : Returns the inverse of the matrix if set else NULL.
##    4. set_inverse() : Sets the inverse of the matrix. It doesn't verify whether inverse is correct or not.

makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  set_inverse <- function(inv_matrix) {
    inverse <<- inv_matrix
  }
  
  get_inverse <- function() inverse 
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Computes inverse and sets it in x, if x doesn'y have invrse set.
## Else returns inverse value set in x. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$get_inverse()
  if (!is.null(inv_matrix)) {
    message("Returning cached value for inverse")
    return(inv_matrix)
  }
  message("Computing inverse of the matrix")
  inv_matrix <- solve(x$get())
  x$set_inverse(inv_matrix)
  inv_matrix
}

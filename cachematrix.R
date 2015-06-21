## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(original.matrix = matrix()) {
  # Check if the input is a matrix
  if (!is.matrix(original.matrix)) {
    stop("Please give a matrix")
  }
  
  # Assign Null to CacheMatrix object
  inverted.matrix <- NULL
  
  # The <<- operator assign a value to an object in an environment 
  # that is different from the current environment. 
  set <- function(y) {
    original.matrix <<- y
    inverted.matrix <<- NULL
  }
}

# Functions for getting and setting cached inv. matrix value
get <- function() original.matrix

# Inversing the matrix using build in solve() function in R
set.inverse <- function(solve) inverted.matrix <<- solve
get.inverse <- function() inverted.matrix

list(
  set = set, 
  get = get,
  set.inverse = set.inverse,
  get.inverse = get.inverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(cacheable.matrix, ...) {
        ## Return a matrix that is the inverse of 'Cacheable Matrix'
  inverted.matrix <- cacheable.matrix$get.inverse()
  # Check if we have cached matrix available.
  if(!is.null(inverted.matrix)) {
    message("Getting cached inverse matrix")
    return(inverted.matrix)
  }
  # Let's create the inverted matrix if
  # there is no cached matrix available.
  matrix.to.inverse <- cacheable.matrix$get()
  inverted.matrix <- solve(matrix.to.inverse)
  cacheable.matrix$set.inverse(inverted.matrix)
  inverted.matrix

}

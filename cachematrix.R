## These functions can be used to calculate the inverse of a square matrix 'x',
## and cache the results.  This can be useful if 'x' is large and the cached
## result can be reused rather than recalculating the inverse.
##
## Example usage:
##  set.seed(8254)
##  m1<-makeCacheMatrix(matrix(sample(1:1000,25),5,5))
##  cacheSolve(m1)
##  cacheSolve(m1) # Note the printed output 'getting cached data' for this second call


## Create a list of functions that can be called later to set/get the matrix,
## and set/get its inverse:

makeCacheMatrix <- function(x = numeric()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve # Calculate the inverse
  getsolve <- function() s
  list(set = set, get = get, # Create the output list of functions
       setsolve = setsolve,
       getsolve = getsolve)
}



## Return a matrix that is the inverse of 'x'; use cached value if present

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  
  if(!is.null(s)) {
    message("getting cached data")
    return(s) # If 's' is not null, then return the cached value...
  }
  
  data <- x$get()
  s <- solve(data, ...) # ...otherwise, get the matrix and calculate its inverse.
  x$setsolve(s)
  return(s)

}
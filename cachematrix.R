## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create functions that allow to set and get values for a matrix and his inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv_l) inv <<- inv_l
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Regresar el dato en cache")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}


##  USAGE:
##  matriz <- makeCacheMatrix(matrix(c(1,3,5,19),2,2))
##  cacheSolve(matriz)

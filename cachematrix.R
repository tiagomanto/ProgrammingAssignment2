## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function creates a matrix object
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL ## inversion variable
  set <- function(y) { #assign values to both
    x <<- y
    inv <<- NULL
  }
  get <- function() x 
  setInverse <- function(inverse) inv <<- inverse # send inverse result to get inverse function
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function
# this function calculate the inverse of matrix, created by 
#function makeCacheMatrix. if the matrix not have been changed
# then it retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) { 
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


#make a test pratice from funtions above

matx <- makeCacheMatrix(matrix(1:20),2,2))
matx$get()
matx$getInverse()
cacheSolve(matx)
cacheSolve(matx)
matx$getInverse()



## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function resets the variables x and inv.
##It passes descriptions of what should be printed when 
##x$get, x$setInverse and x$getInverse are executed
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
##If Inv is not null, then the value that is in Inv will be used
##If null, caclulations need to be run (as per the functions that have been 
##passed to x$get and x$inverse in the MakeCachematrix function
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
 

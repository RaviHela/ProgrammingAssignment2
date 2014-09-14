## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix converts a normal matrix to special Matrix capable of preserving states

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatInv <- function(MatInv) m <<- MatInv
  getMatInv <- function() m
  list(set = set, get = get,
       setMatInv = setMatInv,
       getMatInv = getMatInv)
  
}

## Takes as an argument the special matrix, checks if there is 
##already a inverse of the matrix is stored. If yes it returns
##the stored value and if not creates a new one. It saves the newly created 
## by using setmean() function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getMatInv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setMatInv(m)
    m
  }
 
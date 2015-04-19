## The overall function calculates the inverse of a matrix
## To be more efficient, it caches the result, and will not recalculate
## should the result already exist

## Function to create and compute a list of operands: set, get, setinv, and getinv

makeCacheMatrix <- function(x = matrix()) {
  # sets x equal to an empty matrix
  
  i <- NULL
  # creates a NULL placeholder 'i' to use for the inverse matrix
  
  set <- function(y){
    x <<- y
    # sets x to the incoming argument
    i <<- NULL
    # reset i to NULL if a new argument is defined
  }
  
  get <- function() x
  # returns the matrix x
  
  setinv <- function(solve) i <<- solve
  # computes the inverse matrix
  
  getinv <- function() i
  # returns the inverse matrix
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  # creates the list of operands
}


## This portion checks to see if a result has already been cached, returns the cached
## value, or computes the inverse

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  # get 'i', the inverse matrix calculated above
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
    # if 'i' has already been computer, exit this function with 'i'
  }
  data <- x$get()
  # else get the initial matrix 'x' from above
  i <- solve(data, ...)
  # compute the inverse matrix
  x$setinv(i)
  # store the result in the variable 'i'
  i
  # return the inverse matrix
}  


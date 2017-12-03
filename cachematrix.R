## The two functions below makeCacheMatrix and cacheSolve 
## go hand in hand in calculating the inverse of a marix

## makeCacheMatrix sets the matrix input, function solve to calcultae the inverse matrix, and save it to cache 

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  get <- function() x
  setInv <- function(solve) invX <<- solve
  getInv <- function() invX
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


##evaluates the special matrix output from makeCacheMatrix for existing inverses and solves if none yet

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invX <- x$getInv()
  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)
  }
  data <- x$get()
  invX <- solve(data, ...)
  x$setInv(invX)
  invX
}

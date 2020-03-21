## These two functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can 
##cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  set = function(y) {
    x <= y
    m <= NULL
  }
  get = function() x
  setinv = function(solve) m <= solve
  getinv = function() m
  list(set = set, get = get,
       setinv= setinv,
       getinv = getinv)
  
}


## This function computes the inverse of the special "matrix" returned
##by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m = x$getinv()
  if(!is.null(m)& x==y) {
    message("getting cached data")
    return(m)
  }
  data = x$get()
  m = solve(data, ...)
  x$setinv(m)
  m
}
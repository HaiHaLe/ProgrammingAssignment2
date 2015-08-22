# =============================================================
# makeCacheMatrix function:
#   Create a special "Matrix", which contains functions to
#   - set the value of the Matrix
#   - get the value of the Matrix
#   - set the inverse of the Matrix
#   - get the inverse of the Matrix
# =============================================================
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


# =============================================================
# Solve to get the inverse of the special "Matrix" created with the makeCacheMatrix function.
#   - First checks to see if the inverse has already been calculated.
#   - If so, gets the inverse from the cache and skips the computation.
#   - Otherwise, calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.
# =============================================================

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}

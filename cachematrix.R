# makeCacheMatrix 
# this function Creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
t <- NULL
      set <- function(y) {
            x <<- y
            t <<- NULL
      }
      get <- function() {
            x
      }
      setSolve <- function(solve) {
            t <<- solve
      }
      getSolve <- function() {
            t
      }
      list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)

}

# Function to get the inversed matrix from a object created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
      s <- x$getSolve()
      if(!is.null(t)) {
            message("getting cached data")
            return(t)
      }
      data <- x$get()
      t <- solve(data, ...)
      x$setSolve(t)
      t
}

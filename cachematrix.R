# makeCacheMatrix: returns a list of setters/getters for a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <<- NULL # Declaration of the inverse matrix
  set <- function(y) {
    x <<- y # Overwrite with new matrix
    inverse <<- NULL # Reset the value in case it was cache'd
  }
  get <- function() { x } # Return the original matrix
  setinv <- function(inv) { # Stores the new inverse matrix
    inverse <<- inv
  }
  getinv <- function() { inverse } # Return the inverse matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: computes the inverse matrix and sets it in the cache
cacheSolve <- function(x, ...) {
  inv <- x$getinv() # Get the cache'd value of the inverse matrix
  if(is.null(inv)) { # If the inverse matrix isn't cache'd
    inv <- solve(x$get()) # Compute the inverse matrix
    x$setinv(inv) # Cache the new value
  }
  inv # Return the inverse matrix value
}

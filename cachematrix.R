## This program has two functions. Together, they create a matrix and then find the inverse.


## This function creates the matrix and sets up variables so that the inverse can be 
## found and stored later.
makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) v <<- v
  get_inverse <- function() v
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}

## This function computes the inverse of the above matrix (using the solve function).
## It stores the inverse in a cache.
## It also checks to see if the inverse is stored in the cache before computing a new inverse.

cacheSolve <- function(x, ...) {
  v <- x$get_inverse()
  if(!is.null(v)) {         ## if an inverse is already stored, use that
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data, ...)     ## find the inverse
  x$set_inverse(v)
  v                         ## return the result
}



## makeCacheMatrix creates a special matrix,
## this special matrix is a list containing a function to:
## 1 - set the value of the matrix
## 2 - get the value of the matrix
## 3 - set the value of the inverse
## 4 - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  # return a list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix created by makeCacheMatrix 
## This function first checks if the inverse has already been calculated.
## If so, it gets the inverse from the cache and avoids unnecessary computation.
## If the matrix hasn't been calculated previously, cacheSolve calculates the inverse of the matrix 
# and sets the value of the inverse in the cache with the setinv function.

cacheSolve <- function(x, ...) {
  ## check if the inverse of matrix 'x' is already in cache
  m <- x$getinv()
  if(!is.null(m)) {
    message("inverse of matrix already in cache")
    return(m)
  }
  # If not cached, get the matrix into data aux
  data <- x$get()
  # compute the inverse of data aux
  m <- solve(data, ...)
  # cache the inverse
  x$setinv(m)
  # return the inverse
  m
}
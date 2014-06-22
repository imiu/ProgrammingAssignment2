## This is solution to Programming Assignment 2 of `R Programming` Coursera class
##
## Included function are used to cache computation of matrix inverse
## by storing values in local environment of special cacheable matrix implementation
##
## Usage example:
##
# m <- makeCacheMatrix(matrix(1:4, nrow=2))
# all(m$get() %*% cacheSolve(m) == diag(2)) # gives TRUE
# all(m$get() %*% cacheSolve(m) == diag(2)) # prints "getting cached data" and returns TRUE

## `makeCacheMatrix` crates cacheable matrix of out specified R matrix, or
## when specified with no arguments it creates 1-by-1 cacheable matrix with single NA value.
##
## Result of this function call is "nearly object" (not S3 or S4 object),
## implemented by list of four methods:
##
##  * get() - to get internal R matrix
##  * set(matrix) - to set internal R matrix and reset earlier calculated inverse
##  * getinv() - to get cached inverse, or NULL if no cached inverse is present
##  * setinv(matrix) - to set cached inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # we create cache variable set to NULL
  set <- function(y) { # when we set new matrix
    x <<- y # we replace old x with new value
    i <<- NULL # and reset stored cache
  }
  get <- function() x # to get matrix, we just return x
  setinv <- function(inv) i <<- inv # here we store inverse in cache variable
  getinv <- function() i # and here we return it
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) # what remains is return list of functions, so we can access it from outside world
}

## `cacheSolve` function uses `solve` function to get inverse of cacheable matrix
## returned by `makeCacheMatrix`, but also stores the value for later use.
## If inverse was already calculated, it will return cached result instantly.
## This function passes excess arguments directly to solve method.
cacheSolve <- function(x, ...) {
  inv <- x$getinv() # get cached inverse
  if(!is.null(inv)) { # if it was calculated earlier
    message("getting cached data")
    return(inv) # we have it
  } # otherise
  m <- x$get() # we take R matrix out of cachable matrix
  inv <- solve(m, ...) # and calculate its inverse
  x$setinv(inv) # which we store in cachable matrix
  inv # and return
}

m <- makeCacheMatrix(matrix(1:4, nrow=2))
all(m$get() %*% cacheSolve(m) == diag(2)) # gives TRUE
all(m$get() %*% cacheSolve(m) == diag(2)) # prints "getting cached data" and returns TRUE

## makeCacheMatrix takes a matrix and includes these methods in the object: set, get, setinverse and getinverse## Example:
##
## a <- matrix(c(1,2,3,3,2,1,1,3,3),nrow=3)
##      This isn't new, we are just creating an ordinary matrix, a.
## d <- makeCacheMatrix(a)
##      
## d$get() returns the value of the matrix
## d$set(a) assigns to matrix value of d to a (after a has previously been created), 
##      this also clears out any previous cached matrix inverse
##
## To cache the matrix inverse, use 
## cacheSolve(d)
## This is our second function below.
## d$getinverse() now gives the matrix inverse of the matrix d.
##
## note, if d$setinverse() is used outside of cacheSolve, it will assign whatever values you set as the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

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

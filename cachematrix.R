## The functions do what the assignment asked... seems straightforward...
## makeCacheMatrix makes a matrix object that can cache its inverse
## and cacheSolve computes the inverse or collects the cache if it's already solved

##Usage:
##a=matrix(1:4,2,2)
##obj<-makeCacheMatrix(a)
##inv<-cacheSolve(obj)
##
##a%*%inv will return the identity matrix... tada!

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## Only thing to add to what was already said... requires 
##calling library(MASS) for ginv for the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  library(MASS)
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setinv(m)
  m
}

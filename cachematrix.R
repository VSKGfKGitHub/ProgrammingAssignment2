## The makeCacheMatrix and the cacheSolve functions work together to compute the inverse of a matrix, cache the inverse, and retrieve it from
## the cached inverse (thus saving having to compute it again) if the matrix has not changed


## The makeCacheMatrix takes as its argument an invertible matrix, creates a set of functions that allow setting(caching) and retrieving the 
## inverse of the matrix, and returns a list of the functions it creates
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(m_inverse) m <<- m_inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function takes as its argument the list created by the makeCacheMatrix function, checks to see if the inverse of the associated 
## matrix has been cached, and if so returns the cached inverse; else it computes the inverse, and returns it, as well as caching it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m      
}

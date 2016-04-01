## the first matrix gets and sets the values of a given matrix and and gets and sets values of the inverse of the same matrix
## the second matrix calculates the inverse of a given matrix, but checks first if the inverse has already been calculated in the cahce

##creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  setmat <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmat <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(setmat = setmat, getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}


## this Matrix computes the inverse of the matrix from above, 
## if the inverse has already been calculated, the cacheSolve matrix will retrive it from the cahe

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmat()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

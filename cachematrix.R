## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## we will run this with our matrix before starting

makeCacheMatrix <- function(mat = matrix()) {
  invmat <- NULL
  set <- function(y) {
    mat <<- y
    ## this way we reset the inversed cached matrix when we load a new matrix
    invmat <<- NULL
  }
  get <- function() mat
  setsolve <- function(solve) invmat <<- solve
  getsolve <- function() invmat
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## we are using our matrix, which is now in fact a list (after using it with makeCacheMatrix)
## for example
## mat <- matrix(1:4,2,2)
## preppedmat <- makeCacheMatrix(mat) ## preppedmat is a list
## result <- cacheSolve(preppedmat) ## this will check if we have a cached version or not

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  ## check if we have a cached version already, because this is set to NULL when setting a new matrix in the makeCacheMatrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  print(typeof(mat))
  m <- solve(mat)
  x$setsolve(m)
  m
}

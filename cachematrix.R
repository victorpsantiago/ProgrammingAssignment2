## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  # Set NULL in cache
  cacheX <- NULL

  # create the matrix in the working environment
  set <- function(y) {
    x <<- y
    cacheX <<- NULL
  }
  # get a value
  get <- function() x
  # set in cache the inverse matrix
  setMatrix <- function(inv) cacheX <<- inv
  # get inverted matrix cache
  getInverse <- function() cacheX

  # return the created functions to the working environment
  list(set = set, get = get, setMatrix = setMatrix, getInverse = getInverse)
}

## Write a short comment describing this function

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return the inverse matrix of 'x' if possible in cache
  cacheX <- x$getInverse()

  # if cached
  if (!is.null(cacheX)) {
    message("get cached data")

    return(cacheX)
  }

  # create matrix
  matrix <- x$get()

  # invert
  cacheX <- solve(matrix, ...)

  # set inverted matrix in cache
  x$setMatrix(cacheX)

  # display matrix in console
  return (cacheX)
}

##################################################################################
## The R script contains two functions to effectively cache the inversion
## of a matrix.
## - makeCacheMatrix()
## - cacheSolve()
##
## To use matrix inversion caching you first need to call makeCacheMatrix()
## which sets up and returns a structure to store a cached result.
## The first calls of cacheSolve() will calculate the result and store it in the
## cache. All subsequent calls to cacheSolve() will return the result from the
## cache.

##################################################################################
# The function makeCacheMatrix() creates a function vector to
# - set & get the working matrix
# - set & get the cached inverse of the matrix
makeCacheMatrix <- function(M = matrix()) {
  # initialize the cache
  cachedSolvedMatrix <- NULL
  
  set <- function(newMatrix) {
    # initialize M and cache
    M <<- newMatrix
    cachedSolvedMatrix <<- NULL # clear cache
  }

  get <- function() {
    # return the original matrix
    M
  }
  
  setSolved <- function(solvedMatrix) {
    # set cache
    cachedSolvedMatrix <<- solvedMatrix
  }
  
  getSolved <- function() {
    # return from cache
    cachedSolvedMatrix
  }
  
  # return function vector 
  list(set = set,
       get = get,
       setSolved = setSolved,
       getSolved = getSolved)
}

##################################################################################
# The function cacheSolve() takes the vector returned by makeCacheMatrix()
# as an input and returns the inverse of the contained matrix
cacheSolve <- function(M, ...) {
  # try to find solution in cache
  solvedM <- M$getSolve()
  if(!is.null(solvedM)) {
    # result is cached, so take it and return
    message("getting cached data")
    return(solvedM)
  }
  
  # solution is not yet in cache, so calculate it
  solvedM <- solve(M$get(), ...)
  # store solution in cache and return it  
  M$setSolve(solvedM)
  solvedM
}

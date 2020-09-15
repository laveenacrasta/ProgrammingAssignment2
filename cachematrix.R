## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(a = matrix()) {
      invs <- NULL
      set <- function(){
            a <<- b
            invs <<- NULL
      }
      get <- function() a
      setInverse <- function(solveMatrix) invs <<- solveMatrix
      getInverse <- function() invs
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'x'
      invs <- a$getInverse()
      if(!is.null(invs)){
            message("getting cached data")
            return(invs)
      }
      data1 <- a$get()
      invs <- solve(data1)
      a$setInverse(invs)
      invs 
}



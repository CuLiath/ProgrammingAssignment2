## These two functions work together to
## create a matrix with a memory (caches iteself) and
## retrieve and cache the inverse of the matrix

## makeCacheMatrix does four things:
##	takes a matrix as input and creates a special, cache-able matrix object
##	retrieves the matrix
##	caches the inverse of the matrix
##	retrieves the inverse of the matrix

## cacheSolve operates on a "cachematrix" and
## retrieves the inverse of the matrix if has been cached,
## otherwise calculates it

makeCacheMatrix <- function(x = matrix()) {
	## the matrix is just being created, so we don't have an inverse yet
 	inverse <- NULL
	## the set (sub)function is defined to set the matrix to the input
	## and to set the inverse to null
      set <- function(y) {
      	x <<- y
            inverse <<- NULL
      }
	## the get subfunction takes no argument and returns the matrix
      get <- function() x
	## the setinverse subfunction caches its argument as the inverse
      setinverse <- function(sol) inverse <<- sol
	## the getinverse subfunction retrieves the cached inverse
      getinverse <- function() inverse
      list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
	## note that the argument for this function is not just the matrix
	## but a cache-matrix object as created by makeCacheMatrix
      ## Return a matrix that is the inverse of 'x'
	## First, retrieve the cached inverse
	inverse <- x$getinverse()
	## Check to see if it is null.  
	## If it is not null, print a message that we are retrieving it
	## and return it
      if(!is.null(inverse)) {
           	message("getting cached data")
           	return(inverse)
      }
	## Otherwise, get the matrix
      data <- x$get()
	## solve for its inverse
      inverse <- solve(data)
	## call the setinverse subjection with thhe inverse to cache it
      x$setinverse(inverse)
	## and return the inverse
      inverse
}
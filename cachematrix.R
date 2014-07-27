##############################################################
##  Introduction to R Programming, July 7 to Aug 4, 2014
##  Programming Assignment 2
##  Tim Harig
##############################################################
##  Descripton:  The pair of functions below work together 
##  to compute the inverse of a matrix if it is unavailable
##  or retrieve the inverse from cache if it has already 
##  been found and the original matrix has not changed.
##############################################################
##  Function: makeCacheMwtrix
##  provides a 'set' function to create a cache of a matrix
##  inversion object.
##  provides a 'get' function to supply cached result
##############################################################
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x  ##  supply the matrix
    setinverse <- function(solve) inv <<- solve  ##  cache the result
    getinverse <- function() inv  ##  retrieve cached result
    ##  create list object to hold the get/set functions for cached result
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}
##############################################################
##  Function: cacheSolve
##  Returns a matrix that is the inverse of 'x'.
##  uses functions stored in the environment to check if a cache
##  result is available and returns the cache result it if it is.
##  calls solve() to compute inverse if cache is unavailable.
##############################################################
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()  ##  get cached result
    if(!is.null(inv)) {  ##  if cached result is valid ...
        message("getting cached data")  ##  ... let user know ...
        return(inv)  ##  ... return cached result.  exit function.
    }
    data <- x$get()  ##  if cached result is unavailable, get matrix.
    inv <- solve(data, ...)  ##  create inverse matrix
    x$setinverse(inv)  ##  create a cache of the result
    inv  ##  return result to user
}
##############################################################

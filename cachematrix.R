## Enable caching the result of a function by leveraging lexical scoping in R

## makeCacheMatrix - Function that has 2 data objects (x, ix) and 4 four functions that gets 
## and sets both the data objects, initializing this function onto a object will create a complete copy 
## of the environment(2 data objects and 4 functions) of this function, which in turn enables caching.

makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL
  set <- function (y){
    x <<- y
    ix <<- NULL
  }
  get <- function () x
  setinverse <- function (solve) ix <<- solve
  getinverse <- function () ix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## CacheSolve - Accepts input parameter of type makeCacheMatrix(), stores the inverse of a given matrix
## in makeCacheMatrix() environment and retrieves it if cached if not finds it.

cacheSolve <- function(x, ...) {
  ix <- x$getinverse()
  if(!is.null(ix)){
    message("Getting Cached data")
    return(ix)
  }
  data <- x$get()
  ix <- solve(data, ...)
  x$setinverse(ix)
  ix
}

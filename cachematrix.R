## This file allows to solve and cache the inverse of a matrix. First 
## the matrix stores in an object, with a list of functions to set and get the 
## matrix and its inverse. Then you can call cacheSolve to get the inverse. 
## First time it will be calculated and stored, next times it will get from 
## cache.

## Given a matrix x, makeCacheMatrix returns a list of functions to cache its inverse.
## 'inverse' will store the inverse matrix of x

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  setinv<-function(inv) inverse<<-inv
  getinv<-function()  inverse
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Solves the inverse matrix and store for further use with the 'setinv' defined before

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if (!is.null(inv)){
    message("from cache...")
    return (inv)
  }
  data<-x$get()
  inv<-solve(data, ...)
  x$setinv(inv)
  inv
}

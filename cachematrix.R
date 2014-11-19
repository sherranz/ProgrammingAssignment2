## Put comments here that give an overall description of what your
## functions do

## Given a matrix x, makeCacheMatrix returns a list of functions to cache its inverse
## inverse will store the inverse matrix of x

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


## Solves the inverse matrix and store for firther use with setinv

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if (!is.null(inv)){
    message("from cache...")
    return inv
  }
  data<-x$get()
  inv<-solve(data, ...)
  x$setinv(inv)
  inv
}

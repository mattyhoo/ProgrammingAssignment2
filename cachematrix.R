## Caching the Inverse of a Matrix

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  get<-function()x
  set_inverse<-function(inver_matrix) m<<-inver_matrix
  get_inverse<-function()m
  list(get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  m<-x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$set_inverse(m)
  m    ## Return a matrix that is the inverse of 'x'
}

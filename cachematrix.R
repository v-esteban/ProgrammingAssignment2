## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cInverse<-NULL
  set <- function(y)
  {
    x<<-y
    cInverse<<-NULL
  }
  get <- function() x
  setInverse <- function(solve) cInverse<<-solve
  getInverse <- function() cInverse
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cInverse<-x$getInverse()
  if(!is.null(cInverse))
  {
    message("getting cached data")
    return(cInverse)
  }
  data<-x$get() 
  cInverse<-solve(data,...)
  x$setInverse(cInverse)
  cInverse
}

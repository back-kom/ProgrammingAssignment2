## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL #use this to set a matrix
  set =function(y){
    x <<- y
    inv <<-NULL
    
  }

#it creates a list contains four functions: set, get, setinv, getinv.
get=function() x
setinv=function(inverse) inv <<- inverse
getinv=function() inv
list(set=set,
     get=get,
     setinv=setinv,
     getinv=getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv=x$getinv() #get inversed matrix form x
  if(!is.null(inv)){
    message("getting cached data")
    return(inv) #return the calculated inversion
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setinv(inv)
  inv #return the solved result
        ## Return a matrix that is the inverse of 'x'
}

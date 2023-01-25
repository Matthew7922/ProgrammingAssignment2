## Put comments here that give an overall description of what your
## functions do

## First make a function to store the matrix and its inverse matrix after the 
## further processing function: cacheSolve(). The processed inverse matrix would
## be stored in the first function and being set as a global variable.

makeCacheMatrix <- function(x = matrix()) {
  InvMatrix<-NULL
  set<-function(y) {
    x <<-y
    InvMatrix<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse) InvMatrix <<-inverse
  getInverse<-function() InvMatrix
  list(set=set,get=get,setInverse=setInverse,
       getInverse=getInverse)
}


## The second function would check whether the inverse matrix existed or not. 
## If there's no existed inverse matrix, the "cacheSolve" function would 
## generate the inverse function. After the generation of the inverse matrix,
## the inverse matrix would be stored in the list of the first function and 
## being assign as the global variable.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  InvMatrix<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  } 
  mat<-x$get()
  InvMatrix<-solve(mat,...)
  x$setInverse(InvMatrix)
  InvMatrix
}

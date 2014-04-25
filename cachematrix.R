## This file contains a pair of functions that cache the inverse of a matrix.
## First use function "makeCacheMatrix" on a defined matrix to make a special "matrix" object.
## E.G.: matrix1 <- matrix(data, nrows, ncolumns) N.B.: this should be an invertible matrix!
##       matrix2 <- makeCachematrix(matrix1)
## Then use function "cacheSolve" to cache the inverse of the matrix for later use.
## E.G.: cacheSolve(matrix2)


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

solvematrix<-matrix()
numberrows<<-dim(x)[1]
numbercolumns<<-dim(x)[2]

set<-function(){
  y<<-x
  solvematrix<<-NULL
}

get<-function(){
  return(matrix(y,numberrows,numbercolumns))
}

setinverse<-function(){
  solvematrix<<-solve(y)
}

getinverse<-function(){
  return(matrix(solvematrix,numberrows,numbercolumns))
}

list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then the
## `cachesolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  solvematrix<-x$getinverse()
  if(!is.na(solvematrix[1,1])) {
    message("getting cached data")
    return(matrix(solvematrix,numberrows,numbercolumns))
  }
  
  else 
  x$set()
  x$get()
  x$setinverse()
  solvematrix<-x$getinverse()
  return(matrix(solvematrix,numberrows,numbercolumns))
}

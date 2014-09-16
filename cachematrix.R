## The functions in this R script calculates the inverse
## of an invertible matrix and store the result in cache.
## When later trying to solve the inverse of a matrix, it
## first checks if the solution already exists in cache in
## order to determine the necessity to calculate again.
## The code is based on vector example given in the 
## Coursera course rprog-007 found on
## https://class.coursera.org/rprog-007/human_grading/view/courses/972580/assessments/3/submissions


## The makeCacheMatrix function creates a matrix
## that sets and gets the values of a matrix and
## then sets and gets the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse <- function(solve) m<<- solve
  getinverse <- function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## The cacheSolve function checks if the calculation of
## an inverse matrix exists in cache. If exists, a new
## calculation of the inverse is skipped. If not exist, the
## inverse is calculated and is set in cache. 

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m <- solve(matrix, ...)
  x$setinverse(m)
  m
}

## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  ##inverse property intialization
  i<-NULL
  
  ##set the matrix
  set<- function(matrix){
    m <<- matrix
    i<-NULL
  }
  
  ##get the matix
  get<- function() m
  
  ##set the inverse 
  setInverse <- function(inverse){
    i <<- inverse
  }
  
  ##get the inverse 
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ##get inverse of matrix
  m <- x$getInverse()
  
  
  ##If inverse available just return m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##get the matrix from object
  data <- x$get()
  
  ##calculate the inverse of matrix
  m <- solve(data,...)
  
  ##set the inverse of matrix to object
  x$setInverse(m)
  
  ##Return the matrix
  m
}

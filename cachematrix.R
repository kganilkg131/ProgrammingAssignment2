## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix 
## : To create a special "matrix" object to cache inversed data 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## cacheSolve 
## : To compute inversed matrix which returned by makeCacheMatrix. 


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting data from cache")
    return(m)
  }
  message("computing data")
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}



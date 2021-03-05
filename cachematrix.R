## Put comments here that give an overall description of what your
## functions do





## Write a short comment describing this function

## This function makeCacheMatrix creates a special matrix, 
## which is really a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the values of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
    
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## The cacheSolve function calculates the inverse 
## of the special matrix created with makeCacheMatrix.
## It first checks if the inverse already exists.
## If so, it will get the inverse from the cache 
## and skips the computation.
## Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache
## via setinverse function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse() 
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}


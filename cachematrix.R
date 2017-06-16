
#This it to create  two functions that are used to create a special object 
#that stores a matrix object and cache's its inverse

#The first function makeCacheMatrixr is to create a special "matrix" object 
# that is a list of functions
#1. set the value of the matrix
#2. get the value of the vector
#3.set the value of the inverse of the matrix
#4. get the inverse of the matrix

makeCacheMatrixr <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#This function computes the inverse of the "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
#the inverse from the cache. Otherwise it will calculate the inverse of the matrix
# and set the inverse in the cache via the setsolve function

cachSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  ## Return a matrix that is the inverse of 'x'
  x$setsolve(m)
  m
}
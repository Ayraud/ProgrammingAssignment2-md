#makeCacheMatrix

## The function `makeCacheMatrix` creates a special "matrix", 
## which is really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  compute the inverse of the matrix
## 4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
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

#cacheSolve

## This function computes the inverse of a given matrix. However, it first checks to see if the
## calcuation has already been made. If so, it `get`s the inverse matrix from the
## cache and skips the computation. Otherwise, it calculates the invers of the matrix
## and sets it in the cache via the `setsolve` function.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

## functions and explanation heavily inspired by https://github.com/rdpeng/ProgrammingAssignment2
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Assignment 2 : Lexical Scoping 
# Write an R Function that is able to cache potentially time-consuming computations
# assignment :
# Caching the inverse of a matrix
# 1. makeCacheMatrix (function which will create a special "matrix" which can cache its inverse)
# 2. cacheSolve

# 1

makeCacheMatrix <- function(x = matrix()){
  
  invm <- NULL
  set <- function(y){
    x <<- y
    invm <<- NULL
  }
  
  get <- function() x
  set_inverse <- function(inverse) invm <<- inverse
  get_inverse <- function() invm
  list (set = set, get = get, 
        set_inverse = set_inverse, 
        get_inverse = get_inverse)
}


# 2. create function cacheSolve, the function will look for a matrix and then it will create the inverse of the given matrix from our first
# function. If there is no matrix in the cache, no computation will be performed

cacheSolve <- function(x, ...) {
  invm <- x$get_inverse()
  if(!is.null(invm)) {
    message("getting cached data ...")
    return(invm)
  }
  
  matrix <- x$get()
  invm <- solve(matrix, ...)
  x$set_inverse(invm)
  invm
}
# testing 

matrix1 <- makeCacheMatrix(matrix(1:4,2,2))
matrix1

# showing the matrix
matrix1$get()
# getting its inverse
matrix1$get_inverse()
# getting the data from cache...
cacheSolve(matrix1)



## The inverse of a Square Matrix.
## A Matrix is invertible if it meets the following requirements:
## 1. The determinant of the matrix must not be zero
##    We can use function det(x=matrix,...) to check determinant
## 2. The matrix must be square (same number of rows and columns)
## Note: For this assignment, assume the matrix is ALWAYS invertible.
## If x is square invertible then solve(x,...) function returns its inverse.
##


makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix;   creates and store matrix in cache, if not exists.
  ##                    retrieves matrix from cache, if exists.
  mx <- NULL
  set <- function(y) {
    x <<- y
    mx <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) mx <<- solve
  getInverse <- function() mx
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## CacheSolve function
  ## Condition: If the inverse has already been calculated
  ## AND the matrix has not been changed
  ## Return:  Retrieve inverse of square matrix from the cache
  ## Otherwise; Calculate the inverse of square matrix;
  ##            Set computed inverse in cache
  ##            Return inverse of Square Matrix
  
  mx<- x$getInverse()
  if(!is.null(mx)) {
      message("getting cached inverse of matrix")
      return(mx)
  }
  data <-x$get()
  mx<- solve(data,...)
  x$setInverse(mx)
  mx
        
}

# Tested Functions on two Squared Matrix.
# > source('~/GitHub/ProgrammingAssignment2/cachematrix.R')
# > mxcache <-makeCacheMatrix(matrix(c(7,3,-2,5),2,2))
# > mxcache$get()
# [,1] [,2]
# [1,]    7   -2
# [2,]    3    5
# > cacheSolve(mxcache)
# [,1]       [,2]
# [1,]  0.12195122 0.04878049
# [2,] -0.07317073 0.17073171
# > cacheSolve(mxcache)
# getting cached inverse of matrix
# [,1]       [,2]
# [1,]  0.12195122 0.04878049
# [2,] -0.07317073 0.17073171
# > mxcache$getInverse()
# [,1]       [,2]
# [1,]  0.12195122 0.04878049
# [2,] -0.07317073 0.17073171
# > mxcache$set(matrix(c(10,20,30,40),2,2))
# > mxcache$get()
# [,1] [,2]
# [1,]   10   30
# [2,]   20   40
# > mxcache$getInverse()
# NULL
# > cacheSolve(mxcache)
# [,1]  [,2]
# [1,] -0.2  0.15
# [2,]  0.1 -0.05
# > cacheSolve(mxcache)
# getting cached inverse of matrix
# [,1]  [,2]
# [1,] -0.2  0.15
# [2,]  0.1 -0.05
# > mxcache$getInverse()
# [,1]  [,2]
# [1,] -0.2  0.15
# [2,]  0.1 -0.05
# > 


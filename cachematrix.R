## Matrix inversion being a very compute-intensive operation, 
## caching the inverse of a given matrix may save time and
## resource and thus prevent the the same inverse being computed repeatedly. 

## The following two functions are used to cache the inverse of a matrix.

## The function makeCacheMatrix will take as argument a square matrix x (which is invertible)
## and create and return a list with functions to
##  (a) set the value of a matrix
##  (b) get the value of the matrix
##  (c) set the inverse of the given matrix
##  (d) get the inverse of the given matrix

makeCacheMatrix <- function(x = matrix()) {
  # inverse will store the cached inverse matrix (initialized to null)
  inverse <- NULL
  
  ## function to set the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## function to get the matrix
  get <- function() x
  
  ## function to set the inverse of the corresponding matrix
  setInverse <- function(inv) inverse <<- inv
  
  ## function to get the inverse of the corresponding matrix
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The function cacheSolve takes as argument a list x created by CacheMatrix 
## and checks if the inverse of the corresponding matrix is null or not.
## If the inverse is null it will compute the inverse of the matrix 
## and return the same. 
## If the inverse is already calculated before (i.e. it is not null), 
## it returns the cached inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse of the corresponding matrix
  inverse <- x$getInverse()
  
  ## if inverse is not null, then it need not be computed
  ## the cached value will be returned
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## if inverse is null, it must be computed and returned
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}

## Sample run1:
## 
## Create a 3x3 matrix m
## > m<-matrix(c(1,1/4,2/3,3,5/7,2,4,2/3,5),nrow=3)
##
## Next create a list using makeCacheMatrix, as follows:
## > m1<-makeCacheMatrix(m)
##
## Next calculate the inverse, as follows:
## > cacheSolve(m1)
## [,1] [,2]       [,3]
## [1,] -26.8571429   84 10.2857143
## [2,]   9.6666667  -28 -4.0000000
## [3,]  -0.2857143    0  0.4285714
##
## calculate the inverse again, for the same matrix
## > cacheSolve(m1)
## getting cached data.
## [,1] [,2]       [,3]
## [1,] -26.8571429   84 10.2857143
## [2,]   9.6666667  -28 -4.0000000
## [3,]  -0.2857143    0  0.4285714
##
## Note that the second run will get inverse from cache
##
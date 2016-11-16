## My attempt
## MakeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##cacheSolve
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
## let us create a square matrix
my_matrix<-makeCacheMatrix(matrix(1:4, nrow=2,ncol=2));
##summary of the matrix
summary(my_matrix)
Length Class  Mode    
set        1      -none- function
get        1      -none- function
setInverse 1      -none- function
getInverse 1      -none- function
## Let us test the matrix with our function
my_matrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
my_matrix$getInverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
cacheSolve(my_matrix)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

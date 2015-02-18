## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##  Apparently, this is based on the makeVector example, but I used solve() instead of mean().
##  I still have problems when creating a matrix first (e.g. x <- matrix(...)) and creating 
##  the special makeCacheMatrix object in a second step (e.g. x <- makeCacheMatrix(x)).
##  However, the set of functions works, when the matrix is created in one step.
##  
##  I used this example to verify the functionality:
##  > x <- makeCacheMatrix(matrix(c(1,2,2,3),2,2))
##  > cacheSolve(x)
##        [,1] [,2]
##  [1,]   -3    2
##  [2,]    2   -1
##  > cacheSolve(x)
##  getting cached data
##        [,1] [,2]
##  [1,]   -3    2
##  [2,]    2   -1
##
##
##  PS: you can also try x <- makeCacheMatrix(matrix(rnorm(25),5,5)), but I don't want to verify the result by hand. ;)



makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

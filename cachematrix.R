## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##  Apparently, this is based on the makeVector example, but I used solve() instead of mean().
##  
##  I used this example to verify the functionality (because I can verify it by hand):
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
##


makeCacheMatrix <- function(x = matrix()) {		
  inverse <- NULL												
        ##delete any previously cached values in inverse
  set <- function(y) {								
    x <<- y														
        ##set x outside of function scope
    inverse <<- NULL											
        ##delete inverse value globally outside of function scope
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve	
        ##globally set inverse matrix by assigning computated inverse matrix (see cacheSolve) to variable inverse
  getinverse <- function() inverse						
        ##return stored inverse matrix
  list(set = set, get = get,								
        ##list of method names allowing access via function$method in the form of name = method
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ##return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
        ##assign variable inverse as output from makeCacheMatrix's method getinverse
  if(!is.null(inverse)) {
        ##if inverse has been cached output message and return cached inverse matrix
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
        ##otherwise get the matrix data
  inverse <- solve(data, ...)
        ##calculate inverse via solve() and store in variable inverse
  x$setinverse(inverse)
        ##and set global variable with setinverse method
  inverse
}

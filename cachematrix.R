## These are two functions: The first function (MakeCacheMatrix) stores a matrix
## given as input and also stores ("caches") the inverse of this matrix
## The second function (cacheSolve) checks if the inverse of the matrix has been
## calculated: If it has been, it will print the value stored in the list.
## If it can not find the inverted matrix, it will the calculate the inverse of the
## matrix.

## MakeCacheMatrix: this is a function that creates a list. The list contains 
##  the following elements:
##    set: a function that takes a matrix as an input and sets the value of 
##  x to the input
##    get: this returns the matrix x (the inout iof the function)
##    setinverse: this calculates the inverse of the matrix x
##    getinverse: this returns the inverse of the matrix m


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y=matrix()) {
    x <<- y
    i <<- NULL
  }
  get <- x
  setinverse <- function() i <<- solve(x)
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks if the input of the function (created by makecacheMatrix)
## contains the calculated inverse matrix. If it does, it returns the cached
##matrix. If not, it then calculates the inverse of the original matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    print("Printing cached matrix")
    return(i)
  }
  
  data <- x$get
  i <- solve(data)
  x$setinverse()
  i
}

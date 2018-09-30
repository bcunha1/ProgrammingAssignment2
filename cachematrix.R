
## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function

# The function makeCacheMatrix:
#(1) make a matrix as an input, 
#(2) then set the matrix value,
#(3) then get the matrix value,
#(4) then set the inverse matrix,
#(5) then get the inverse matrix.


makeCacheMatrix <- function(x = matrix()) {         #(1)
  inv <- NULL
    set <- function(y){                             #(2)
    x <<- y
    inv <<- NULL
  }
  get <- function () x                              #(3) 
  setinverse <- function (inverse) inv <<- inverse  #(4) 
  getinverse <- function () inv                     #(5) 
  list (set = set,
        get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function

## The function cacheSolve:
#(1) takes as an input the makeCacheMatrix function; 
#(2) checks if the inverse matrix is empty;
#(3) if it has any value because the function already run  one time, it writes the message "getting cached data" 
#(4) show the result of the inverse matrix form cache;
#(5) if it hasn't any value, it takes the original matrix;
#(6) calculate its inverse;
#(7) set the matrix inverse;
#(8) return the matrix inverse;


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()                     #(1)
  if(!is.null(inv)) {                       #(2)
    message("getting cached data")          #(3)
    return(inv)                             #(4)
  }
  data <- x$get()                           #(5)
  inv <- solve(data, ...)                   #(6)
  x$setinverse(inv)                         #(7)
  inv                                       #(8)
}

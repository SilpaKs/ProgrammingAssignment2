## cacheSolve(makeCacheMatrix(x = matrix()))

## makeCacheMatrix function 
## input <- any invertible square matrix
## output <- a special "vector", which is a list containing a function to
##      1.  set the value of the vector
##      2.  get the value of the vector
##      3.  set the value of the inverse
##      4.  get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y){
    x <<- y 
    inv <<- NULL
  }
  get <- function() x
  
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## calculates the inverse of the special "vector" created with the makeCacheMatrix(). 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it `get`s the inverse from the  cache via 'getinverse' function and 
## skips the computation. Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

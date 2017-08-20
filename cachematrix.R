## There are 2 functions in this file, which are makeCacheMatrix and cacheSolve
## They work together to find out a matrix's inverse and store the result in the memory
## instead of re-calculating it.  Therefore, the retrieval time could be shortened. 



## makeCacheMartix allows user to create a matrix.
## Actually, it returns a list of four functions.
## The get and set functions allow user to set and retrieve the created matrix.
## The getinverse and setinverse are built for the cachSolve function,
## which allow the cashSolve function to check the existence of the matrix's inverse in the memory
## and save the calculated inverse in the memory.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  getinverse <- function() {
    inv
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
  # Return a list containing the set, get, setinverse and getinverse function
}


## cachSolve function take the matrix created by makeCacheMatrix, calculate the corresponding inverse,
## save the calculated inverse in the memory and get the cached inverse instead of recalculating
## the it, provided that it has been in the memory already.

cacheSolve <- function(x, ...) {
  inv = x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

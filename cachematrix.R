## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix(), taking a matrix as its argument, sets i <- NULL, 
## defines four functions: set(), get(), setinverse(), and getinverse(),
## and creates a named list with those four functions.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
          x <<- y
          i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve(), taking a list created by makeCacheMatrix as its argument,
## retrieves i through the getinverse() function.
## If i is not NULL, it displays"getting cached data" and returns i. 
## If i is NULL, it 
##    Retrieves the matrix using get(),
##    Calculates the inverse of the matrix using solve(),
##    Caches the inverse using setinverse(), and
##    Prints the inverse

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

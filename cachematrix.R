## The functions store a matrix instance and its inverse (if calculated)
## An inverse of a matrix can be calculated or restored from a cached instance


## This function holds getters and setters for a matrix, a method that 
## assigns its inverse to a variable (cache) and another method that
## retrives the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(mat) {
    x <<- mat
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inversed) inv <<- inversed
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes an inverse of a matrix if it is not available in
## cache, otherwise it retrieves the inverse from a cached object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

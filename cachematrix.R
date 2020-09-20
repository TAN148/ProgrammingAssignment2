##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.If the inverse has already been calculated (and the matrix has notchanged), then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  d <- NULL
  set <- function(y){
  x <<- y
  d <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}

## The following function calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  d <- x$getInverse()
  if(!is.null(d)){
  message("getting cached data")
  return(d)
  }
  mat <- x$get()
  d <- solve(mat,...)
  x$setInverse(d)
  d
}

}


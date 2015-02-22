## This function set "y" as a vector named "originalMatrix" and a vector 
## named "inverseMatrix" as null.
## Then, it is used to cache the data which were got.

makeCacheMatrix <- function(originalMatrix = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    originalMatrix <<- y
    inverseMatrix <<- NULL
  }
  get <- function() originalMatrix
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function is used to verify if there's a inverse matrix data. 
## If it has, returns the data cached, 
## If not, get matrix, calculates its inverse data, cache that value and returns the value. 

cacheSolve <- function(cacheMatrix) {
  inv <- cacheMatrix$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- cacheMatrix$get()
  inv <- solve(data)
  cacheMatrix$setinverse(inv)
  return(inv)
}

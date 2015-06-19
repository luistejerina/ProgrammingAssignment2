## This function caches a matrix inverse




makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {#changes vector stored in main
    x <<- y#two << because it changes the vector in the main function
    m <<- NULL
  }
  get <- function() x#returns vector given as arg of makevector
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This second function checks if the matrix inverse has been calculated and
## gets the one in the cache if it has, if not it recalculates the inverse


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m        ## Return a matrix that is the inverse of 'x'
}


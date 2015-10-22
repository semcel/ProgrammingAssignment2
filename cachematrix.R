## makeCacheMatrix supplies functions and variables to 
## create and manage matrix and its inverse matrix
## with a simple cache structure

## Write a short comment describing this function

## creation of empthy matrix as default
makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
  
  ## set input parameter y to stored matrix x
  set <- function(y) {    
    x <<- y
    m <<- NULL
  }
  ## return stored matrix
  get <- function() x
  
  ## assign input parameter solve to cached inverse matrix m
  setinverse <- function(solve) m <<- solve
  
  ## return cached inverse matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## return cached inverse matrix or (if it is not cached) calculates it
cacheSolve <- function(x, ...) {
  ## get inverse matrix from cache
  m <- x$solve()
  
  ## if cache is empty
  if(!is.null(m)) {
    message("getting cached data")
    return(m)## return inverse matrix from cache
  }
  
  ## if cache is empty, calculate inverse
  data <- x$get()
  m <- solve(data, ...)
  
  ## put calculated inverse to cache
  x$setinverse(m)
  
  #return inverse matrix
  m
}

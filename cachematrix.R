## Two functions created: makeCacheMatrix and cacheSolve.
## makeCacheMatrix caches a given matrix
## cacheSolve creates the inverse of a given matrix, 
## either by solving or recalling from cache

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL ##i is the inverse matrix, to be created below
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## assigns matrix value
  get <- function() x
  ## obtains matrix
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i
  ## sets and gets inverse of matrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Returns a matrix that is the inverse of 'x': 
## very closely based on mean example given in assignment

#Test 
M <- matrix(c(2,4,6,8), 2, 2)
solve(M)
## Yields:
## [,1]  [,2]
## [1,] -1.0  0.75
## [2,]  0.5 -0.25

## Cache M
M1 <- makeCacheMatrix(M)
## Compute inverse
cacheSolve(M1)
##      [,1]  [,2]
## [1,] -1.0  0.75
## [2,]  0.5 -0.25

## Retrieve inverse from cache
cacheSolve(M1)
## getting cached data
##      [,1]  [,2]
## [1,] -1.0  0.75
## [2,]  0.5 -0.25
#Creating a function that will contain a list of functions (set, get, setinverse, getinverse)
#and will take an invertable matrix x as an argument

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
          
# this function will take makeCacheMatrix as argument and will return inverted matrix
# either from cache, or will calculate it and set the value in cache for future fuction calls.
          
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } else
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#example
> myMatrix <- makeCacheMatrix(x = matrix(4:1, 2, 2))
> cacheSolve(myMatrix)
     [,1] [,2]
[1,] -0.5    1
[2,]  1.5   -2
> cacheSolve(myMatrix)
getting cached data
     [,1] [,2]
[1,] -0.5    1
[2,]  1.5   -2

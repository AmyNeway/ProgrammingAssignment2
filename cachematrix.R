makeCacheMatrix <- function(x = matrix()) {
  #makeCacheMatrix is assigned a function matrix
  #m is NULL
  #makeCacheMatrix is just holding the function in memory and not calculting its inverse at this point.
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  #getting the function from cache
  #set contains the data we need to work with, setinverse will hold the inverted matrix
  #getinverse will return the inverse of the orginal get data
  
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  #get the value of the inverse
  
  m <- x$getinverse()
  if(!is.null(m)) {
    #if m is not null then give me the message "getting cached data", and give me the result for m when it gets calculated.
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  # makeCachematrix is basically the data
  # calculate the inverse of the data
  # save the result into memory
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
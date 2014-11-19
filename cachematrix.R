## This R file is my submission for Assignment2 of Coursera

## This function gives a matrix that can cache itself

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the value where we will store the inverese
  i <- NULL 
  
  ## Sets the value of the vector
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## gets the value of the vector
  get <- function() x
  
  ## sets the value of the inverse
  setinverse <- function(inverse) i <<- inverse
  
  ## gets the value of the mean
  getinverse <- function() i
  
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  
  ## first of all we check if the inverse is computed 
  if(!is.null(i)){
    
    ## if the inverse is computed, we return it and stop
    message("getting cached data") 
    return(i)
  }
  
  # if the inverse is not computed
  
  data <- x$get()
  i<- solve(data, ...) # 
  x$setinverse(i) # sets the value of the inverse in the cache
  
  ## this function returns the inverse
  i
  
}

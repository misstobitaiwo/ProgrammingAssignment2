# These are two functions that create a special object that stores a matrix and 
#cache's its inverse.


#The makeCacheMatrix function below, creates a special matrix object that can
#cache its inverse. It returns a list that contains a function to;

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(matrix) {
    x <<- matrix
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#The cacheSolve function below returns the inverse of the matrix using the list
#produced from the above function. First, it checks if the inverse has been
#calculated. If yes, it just returns the inverse. If no, it computes the inverse
#of the matrix and sets the value of the inverse in the cache using the function
#below.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

##  1.set the value of the vector
  
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  
##  2.get the value of the vector
  
  getmatrix <- function() x
  
##  3.set the value of the inversed matrix
  
  setimatrix <- function(imatrix) m <<- imatrix
  
##  4.get the value of the inverse matrix
  
  getimatrix <- function() m
  
## Assign the functions to a list
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setimatrix = setimatrix,
       getimatrix = getimatrix)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
## check whehter the matrix has been inversed and saved
  
  m <- x$getimatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
## Retrieve the matrix
  
  data <- x$getmatrix()
  
## inverse the matrix
  
  m <- solve(data, ...)
  
## Save the inversed matrix
  
  x$setimatrix(m)
  
## Return inversed matrix
  
  m
  
  
}

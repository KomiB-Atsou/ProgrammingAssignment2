## Komi ATSOU
## Coursera R programmming course

## These functions help compute the inverse of a matrix using cache 
# capabilities of R

## This function returns a list of functions for cache purposes

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize inverse as NULL
  ## will further hold the value of the inverse of matrix 
  inv <- NULL
  
  ## Set function to assign value of matrix in parent environment
  ## and re-initialize the inverse
  set <- function(y) {  
    ## Assign value of matrix in parent environment
    x <<- y   
    ## After the value of the matrix is reset, we 
    ## have to re-initialize the inverse
    inv <<- NULL         
  }
  
  ## Returns value of the matrix argument
  get <- function(){
    x
  }
  
  ## Assigns value of inv in parent environment
  setinverse <- function(inverse){
    inv <<- inverse
  }
  
  ## Gets the value of inv
  getinverse <- function(){
    inv
  }
  
  ## Return the list of functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function returns the inverse of a matrix using cache capabilities of R
## implemented in makeCacheMatric fuction

cacheSolve <- function(x, ...) {
  
  ## Use the function getinverse to return the inverse 
  ## of matrix x if it is already computed and cached
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse is not already computed, compute and return it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
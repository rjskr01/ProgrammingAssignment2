
## This function create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  #variable declaration for inverse matrix
  inverse <- NULL
  
  # set the matrix value as x in environment
  set <- function(y)
  {
    x<<-y
    inverse <<- NULL
  }
  
  # get the matrix value
  get <- function()
  {
    x
  }
  
  # set the inverse matrix 
  setinverse <- function(inverse)
  {
    inverse <<- inverse
  }
  
  #get the inverse matrix value
  getinverse <- function()
  {
    inverse
  }
  
  list(set = set, get = get ,setinverse =setinverse ,getinverse =getinverse)
  
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        
        #Check the inverse value computation already done or not
        
        if(!is.null(inverse))
        { 
          message("Getting cached matrix value")
          return(inverse)
        }
        # Incase invesrse matrix computation not completed , 
        # Have to get the original matrix and calculate the inverse matrix
        data <-x$get()
        
        #Calculate the inverse matrix
        inverse <-solve(data)
        #set the inverse matrix value
        x$setinverse(inverse)
        inverse
}
## starting the code with a NULL matrix argument 
makeCacheMatrix <- function(x = matrix()) {
      
	# additional warning message to ensure input is a invertible matrix   		
	if(nrow(x)!= ncol(x) || det(x) == 0) stop ("not a valid input for inversion")
	
	# setting the inverse value to null in the beginning 
	i <- NULL
	
	# defining another function to reset the value of variable "i" 
	# (which caches inverse value) in case of input changes 
      set <- function(y) {
      x <<- y
      i <<- NULL
      }
	
	# returns the currently stored given matrix
      get <- function() x
	
	# stores (caches) the inverse value in i 
      setinv <- function(inv) i <<- inv
	
	# returns the currently stored inverse value of given matrix 
      getinv <- function() i
	
	# passing functions to the names which will be exposed to the 
	# outside classes/functions for calling
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## this function is used to compute inverse OR retrieve cached values
cacheSolve <- function(x, ...) {
      
	# get inverse is called on the output of "makeCacheMatrix" to get the 
	# currently saved value of inverse 
	i <- x$getinv()
      
	# incase the value is cached, returning the inverse directly
	if(!is.null(i)) {
      message("getting cached matrix inverse")
      return(i)
      }
      
	# incase inverse is not cached, matrix x is stored in data
	data <- x$get()
      
	# inverse is calculated using solve function and stored in i
	i <- solve(data, ...)
      
	# newly computed inverse is stored in i (now this inverse is cached)
	x$setinv(i)
      
	# returning the inverse
	i

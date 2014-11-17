## This is a pair of functions intended to cache the inverse of a matrix
## to be used when needed.

## `makeCacheMatrix` creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL  #inv will store the inverse of our matrix initially 
                  #set to NULL until called upon by cacheMatrix

      set <- function(y) {    #set function to reset inv to NULL when
            x <<- y           #called
            inv <<- NULL
      }
      
      get <- function() x     #returns the initial matrix
      
      setInv <- function(solve) inv <<- solve #function to set the inverse
                  #of x when called by cacheSolve and store the value
      
      getInv <- function() inv #retireves the value of inv on subsequent 
                              #calls by cacheSolve
      
      list(set = set, get = get,  
           setInv = setInv,
           getInv = getInv) #creates a list for calling function to access
                              #internal function in makeCacheMatrix
}


## `cacheSolve` computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {        
      inv <- x$getInv()       #retrieves current value of inv
      if(!is.null(inv)) {     #if inv is not NULL then returns the cached
            message("Fetching previously cached inverted matrix.")  
            return(inv)       #inverse of the matrix 'x'
      }
      data <- x$get()   #if inv == NULL then retrieve uninverted matrix
      inv <- solve(data, ...) #invert the matrix
      x$setInv(inv)     #set the value of inv == inverted matrix
      inv         # Return a matrix that is the inverse of 'x'
}

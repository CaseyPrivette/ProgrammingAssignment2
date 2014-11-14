## This is a pair of functions intended to cache the inverse of a matrix
## to be used when needed.

## `makeCacheMatrix` creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

}


## `cacheSolve` computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

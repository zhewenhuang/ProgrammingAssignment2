##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
#assign NULL to the initial cache
cacheMatrix <- NULL
#set the value of matrix
setMatrix <- function(y) {
x <<- y
cacheMatrix <<- NULL
  }
#get the value of matrix
getMatrix <- function() x
#set the value of inverse of matrix
set_inv <- function(inverse) cacheMatrix <<- inverse
#get the value of inverse of matrix
get_inv <- function() cacheMatrix
        
list(setMatrix = setMatrix,
     getMatrix = getMatrix,
     set_inv = set_inv,
     get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

# This function will return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
cacheMatrix <- x$get_inv()
#check if inverse of matrix has already been calculated, if yes, return the result
if (!is.null(cacheMatrix)) {
message("getting cached matrix...")
return(cacheMatrix)
  }
#if inverse of matrix has not been calculated, then calculate and set the value of inverse in the cache via the setCache function
invMatrix <- x$getMatrix()
cacheMatrix <- inverse(invMatrix, ...)
x$set_inv(cacheMatrix)
return(cacheMatrix)

}

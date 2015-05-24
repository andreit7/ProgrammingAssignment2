## 'makeCacheMatrix' function creates a special matrix that can cache its inverse
## 'cacheSolve' function caches and calculate inverse of a special matrix created with 'makeCacheMatrix'

## 'makeCacheMatrix' creates matrix and four functions which you can use to 
##  get/set matrix and get/set inverse of a this matrix

makeCacheMatrix <- function(x = matrix()) {
  invertedmatrix <- NULL
  set <- function(y) {
    x <<- y
    invertedmatrix <<- NULL
  }
  get <- function() x
  setinverted <- function(inverted) invertedmatrix <<- inverted
  getinverted <- function() invertedmatrix
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted
  )
  
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverted()
        if(!is.null(inverse)) {
          message("getting cached inverse of a matrix")
        } else {
          data <- x$get()
          inverse <- solve(data, ...)
          x$setinverted(inverse)
        }
        inverse
  
}

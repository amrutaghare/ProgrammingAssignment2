###two functions that are used to create a special object that stores a 
##matrix and cache's its inverse

#makeCacheMatrix returns: a matrix with functions to
#get/set values and inverse

makeCacheMatrix <- function(x = matrix()) {
  #cached inverse of matrix
  inv <- NULL
  #gets/sets the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  #gets/sets the inverse matrix
  setinvmat <- function(solve) inv <<- solve #calculates the inverse
  getinvmat <- function() inv
  #return list of functions for matrix
  list(set = set, 
       get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}

#gets the inverse of a matrix, if already calculated, returns the cached value
cacheSolve <- function(x, ...) {
  
  inv <- x$getinvmat()
  ##return cached value if already calculated
  if(!is.null(inv)) {
    message("getting cached data of inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinvmat(inv)
  return(inv)
}

###    ******EXAMPLE***** 
##defining the matrix
data<-matrix(1:4,nrow=2,ncol=2)
data2<-makeCacheMatrix(data)
###sets the matrix
data2$set(data)
##gets the matrix
data2$get()
##solves the inverse
cacheSolve(data2)
## if run again, returns cached value with message
cacheSolve(data2)

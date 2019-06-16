#The two functions below are functions that cache and compute the inverse of a matrix.

#This function below will create a special matrix object that can cache its inverse.

makeCacheMatrix <- function(matr = matrix()) {
  inverse <- NULL
  set <- function(x) {
    matr <<- x
    inverse <<- NULL
  }
  get <- function(){
    return(matr)
  }
  setinv <- function(inv){
    inverse <<- inv
  }
  getinv <- function(){
    return(inverse)
  }
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

#The function below calculates the inverse of the special matrix returned by `makeCacheMatrix` above. If the inverse has
#been calculated (and the matrix remained unchanged), then `cacheSolve` function should retrieve the inverse from the cache.

cacheSolve <- function(matr, ...) {
  inverse <- matr$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- matr$get()
  inverse <- solve(data, ...)
  matr$setinv(inverse)
  return(inverse)
}

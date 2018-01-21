


##function to cache the inv of matrix

makeCacheMatrix <- function(x = matrix()) {

  
  inverted.matrix <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  
  get <- function() x
  
  set.inverse <- function(data) inv <<- data
  get.inverse <- function() inv
  
  list(
    set = set, 
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)
  
}



## Function to check if we have a valid inv or will recalc the inv of matrix

cacheSolve <- function(cacheable.matrix, ...) {
  inv <- cacheable.matrix$get.inverse()
  
  #check if we have a cahced version
  if(!is.null(inv)) {
    message("Getting cached inverse matrix")
    return(inv)
  }
  # there's no cached matrix available.
  matrix.to.inverse <- cacheable.matrix$get()
  inv <- solve(matrix.to.inverse)
  cacheable.matrix$set.inverse(inv)
  inv
  
}
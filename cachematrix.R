## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function returns a list containing function to get/set a matrix 
##  and getin/setinv to inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv_x <<- inv
  getinv <- function() inv_x
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
## cacheSolve will calculate inverse of the matrix and save to the cache during 1st time and
## return the inverse from cache if the matrix doesnt change and if value in cache exists
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinv()
  ## check if inverse exist in the cache
  if(!is.null(inv_x)){
    message("getting cached matrix")
    return(inv_x)
  }
  ## cache doesnt contain inverse so need to calculate the inverse and save it in the cache
  m_x <- x$get()
  inv_x <- solve(m_x)
  x$setinv(inv_x)
  inv_x
}

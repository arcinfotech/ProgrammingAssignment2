## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix<-function(x = matrix()) {

#function to cache 
  m <- NULL
  set <- function(y) {
    x<<- y
    m <<- NULL
  }
  
#get cached
  get <- function() x

#create inverse matrix and cache
  i_matrix <- solve(x)
  setinverse <- function(i_matrix) m <<- i_matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
 }



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
##get cached matrix from function makeCacheMatrix
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
 
#create inverse if matrices are not the same
  if (!isTRUE(all.equal(x,m))) {    
      m <- solve(x)
  }
#matrix unchanged returned the cached matrix
    else 
      i_matrix<- o_matrix   
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      m  

  ##return inverse matrix
  return(m)
}

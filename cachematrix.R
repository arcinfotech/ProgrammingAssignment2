## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix2<-function(x = matrix()) {
  mrows <- nrow(in_m)
  mcols <- ncol(in_m)
  im_vec <- c(x)
  vlen <- length(im_vec)
#create inverse of input matrix
  inverse_v <- im_vec * t(im_vec)
  i_matrix <- matrix(data = inverse_v, nrow = mrows, ncol = mcols)

#cache input matrix
  o_matrix <<- i_matrix
  return(o_matrix)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##  if the input is the same as inverse, no change then return the cached matrix else return the inverse
cacheSolve <- function(x, ...) {
  mrows <- nrow(x)
  mcols <- ncol(x)
i_matrix<- matrix(data = NA, nrow = mrows, ncol = mcols)
i_matrix <- NULL

  if (is.matrix(x) ) {
##compute the inverse for comparison with the input matrix
   cm <- makeCacheMatrix2(x)
   v_cm <- c(cm)
   v_x <- c(x)
##check if input and inverse have not been changed
    if (!isTRUE(all.equal(v_x,v_cm))) {    
      i_matrix <- makeCacheMatrix2(v_x)
    }
    else 
 ## Get cached matrix because input and inverse are the same.
      i_matrix<- o_matrix   
  }
  ##return inverse matrix
  return(i_matrix)
}

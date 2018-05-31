## We want to create a special matrix object which can cache its inverse and hence finding the inverse of the matrix will be computationally easier



## This function created a special "matrix" object which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	
	get <- function() x
	set_inverse <- function(inv) inverse <<- inv
	get_inverse <- function() inverse
	list(set = set, get = get,set_inverse = set_inverse,get_inverse = get_inverse)
	
}


## This function computes the inverse of the special "matrix" object returned by the above function
cacheSolve <- function(x, ...) {
     inverse <- x$get_inverse()
     if(!is.null(inverse)){
     	return(inverse)  ## if inverse is already cached then return else find inverse using solve function of R
     }   
     mat <- x$get()
     inverse <- solve(mat,...)
     x$set_inverse(inverse)
     inverse
}

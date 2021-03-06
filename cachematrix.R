## Programming Assignment 2 for R Programming

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL  ##sets inverse to NULL as placeholder for future inverse

	## the set function allows you to define the matrix. The global << is used  
	## so you can use the object outside of the function
	set <- function(y) {
		x <<- y
		inverse <<-NULL  ##looks in parent environment
	}
	
	##returns the matrix x
	get <- function() {
		x
	}    	##can also be written in shorter format get <-function()x 

	## the setinverse function is called in the cacheSolve function assigning
	## the inverse. Hence global << is required so it can be called outside	
	setinverse <- function(inverse) inverse<<-inverse 

	## get function returns the inverse
	getinverse <- function()inverse

	## returns the special vector containing all four functions.
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## and the matrix has not changed, then the cacheSolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {

	## Returns a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	
	## if the inverse matrix is not empty then obtain the cached inverse
	## from makeCacheMatrix

	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}

	##otherwise calculate the inverse and set it in the makeCacheMatrix

	data <- x$get()
	inverse <-solve(data,...)
	x$setinverse(inverse)
	inverse
}

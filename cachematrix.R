## The function makeCacheMatrix create a special matrix object that caches 
## its inverse. This functiona essentially returs a list of functions 
## containing functions to 
##	 - set the matrix
##	 - get the matrix
##	 - set the inverse of the matrix
##	 - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	mInverse <- NULL
	set <- function(y){
		x <<- y
		mInverse <<- NULL
	}
	get <- function() x
	setinverse <- function( matinverse ) mInverse <<- matinverse
	getinverse <- function () mInverse
	list(set= set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of a matrix using the special 
## matrix object created with the funciton above. It first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse 
## of the matrix and sets the value of the inverse in the cache using 
## setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

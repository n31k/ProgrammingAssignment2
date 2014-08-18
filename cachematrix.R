## These are two functions written as a course assignment.  
## The first function returns a list of appropriate functions
## of the input, with which a matrix x is hold, and its inverse
## can be calculated and cached. For usage, consult the respective commments
## above each function.

## In order to use this function, assign it to a name in the workspace (e.g. "a").
## Then "a" is a list of functions. Usage: a <- make CacheMatrix() ;
## a$set(X), where X is an invertible matrix; this will set the matrix argument to be X
## a$setinverse() , which will assign the inverted matrix in the right place in the list to
## hold, accesible with a$getinverse().

makeCacheMatrix <- function(x = matrix()) {
	minv <- NULL
	set <- function(y){
		x <<-y
		minv <<- NULL
	}
	get <- function() x
	setinverse <- function(x) minv <<- solve(x)
	getinverse <- function() minv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function takes an object created by makeCacheMatrix, and applies
## the appropriate functions in the list so that it uses the cached inverse
## if available, otherwise computes and caches it. For example: 
## X <- matrix(runif(100), 10, 10);
## a <- makeCacheMatrix(X); cacheSolve(a) ; the first time it computes the 
## inverse from scratch and returns it; but if run again cacheSolve(a), it will 
## find the cached inverse and return it from memory.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	minv <- x$getinverse()
	if(!is.null(minv)){
		message("getting cached data")
		return(minv)
	}
	data <- x$get()
	minv <- x$setinverse(data)
	return(minv)
	
}

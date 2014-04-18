## Inverting a matrix can be computationnally intensive
## If we invert the same matrix multiple times, it can be worth caching the result

##        X = matrix(data=c(2,0,0,2),nrow=2,ncol=2)
##        M = makeCacheMatrix(X)
## Creates a special matrix M and caches its inverse
##
##        N = cacheSolve(M)
## computes the inverse of the special matrix M
## if the inverse has already been computed
## gets it from cache

## Mostly taken from the example "Caching the Mean of a Vector"

## this function creates a special matrix, which is a list containing
## a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(X = matrix()) {

	# A contains the value of the inverse, if cached

	A <- NULL

	# set: set value of the matrix, set inverse to NULL
	set <- function(Y) {
		X <<- Y
		A <<- NULL
	}

	# get: get the value of the matrix
	get <- function() X

	# set and get value of the inverse
	setInverse <- function(B) A <<- B
	getInverse <- function() A
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

## this function returns the inverse of a special matrix created
## with the makeCacheMatrix function
## either by computing it or by getting the result from cache

cacheSolve <- function(X, ...) {
        
	# first, try to get inverse from cache
	M <- X$getInverse()
	if(!is.null(M)) {
		# if stored inverse is not null, it has already been computed
		# -> get value from cache
		message("getting cached data")
		return(M)
	}
	# if stored inverse is null, compute the inverse and store it for eventual later usage
	data <- X$get()
	M <- solve(data, ...)
	X$setInverse(M)
	M
}

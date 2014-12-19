## The problem is solved using two functions:
## makeCacheMatrix and cacheSolve
## The first one creates a list object containing a square matrix, cashed 
## inverse of the matrix and functions to manipulate the data
## The other function retrievs the inverse matrix if it has been cached and
## if not - calculates and caches the inverse.
## Minimal example:
##
## source("cachematrix.r")
## t <- makeCacheMatrix(matrix(rnorm(9),3))	#Create matrix
## tt<- cacheSolve(t)				#Calculate and cache its inverse
## tt<- cacheSolve(t)				#Retrieve the inverse from cache



## makeCacheMatrix takes a square inversible matrix x as an argument and 
## creates a list object with x, its cached inverse and several functions for
## manipulating the data:
## get     - to get the x data,
## set     - to change the matrix data
## setinv  - to set the inverse
## getinv  - to retrieve the inverse from cache 

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)


}


## cacheSolve checks if there is a cached inverse for the
## matrix. If there is - it retrieves it from the cache, otherwise
## it computes the inverse and caches it

cacheSolve <- function(x, ...) {
	  m <- x$getinv()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$get()

	  ##Following code can be uncommented if one wants to check
	  ##that the matrix is square and not singular
	  ##if (dim(data)[1]!=dim(data)[2]) {message("Matrix is not square"); return()}
	  ##else if (det(data)==0) {message("Matrix is singular"); return()}
        m <- solve(data, ...)
        x$setinv(m)
        m
}

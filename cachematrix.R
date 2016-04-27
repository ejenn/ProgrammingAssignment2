## These functions return the inverse of a given invertible matrix
## The matrix must be a square matrix, ex. 2 rows x 2 cols, 3 rows x 3 cols, etc.
## If the inverse of a given matrix has previous found,
##  it will return the cached inverse that was previously returned.

## Store the result of makeCacheMatrix(matrix()) in a variable, then call the variable with cacheSolve(x, ...)
## Ex.:
##		var <- makeCacheMatrix(matrix(c(1,2,3,4),nrow = 2, ncol =2))
##		cacheSolve(var)
##		cacheSolve(var)
## When you perform cacheSolve(var) the second time, it will pull from cache.

## creates a matrix object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
				x <<- y
				m <<- NULL
		}
		get <- function() x
		setinverse <- function(solve) m <<- solve
		getinverse <- function() m
		list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## computes the inverse of the matrix returned by makeCacheMatrix, and returns a precalculated inverse if the unchanged matrix has been previously inversed:

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
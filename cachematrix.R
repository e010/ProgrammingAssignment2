## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the solve, which is inverse of matrix
## 4. get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## Checks to see if there's an inverse of the matrix cached,
## If there is, then retrive it to return, otherwise, call the solve function and return the inverse.
## Assumption: it's a square matrix that could be inversed with the solve funciton

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getmean()
        if(!is.null(inverse)) {
                message("cached data is not null, getting it to return")
                return(inverse)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

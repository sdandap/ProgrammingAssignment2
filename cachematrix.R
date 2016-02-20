## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Saves the matrix to variable x and its inverse to variable invX in scope.
makeCacheMatrix <- function(x = matrix()) 
{
        invX <- NULL
        set <- function(y) 
        {
                x <<- y
                invX <<- NULL
        }
        get <- function() 
        {
                x
        }
        setSolve <- function(solve) 
        {
                invX <<- solve
        }
        getSolve <- function()
        {
                invX
        }
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Write a short comment describing this function
# This function will get the inversed matrix from an object created by makeCacheMatrix.
cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        invX <- x$getSolve()
        if(!is.null(invX)) 
                {
                message("getting cached matrix invX")
                return(invX)
                }
        data <- x$get()
        invX <- solve(data, ...)
        x$setSolve(invX)
        invX
}

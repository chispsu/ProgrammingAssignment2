## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


## Write a short comment describing this function

## makeCacheMatrix creates a special “matrix” object that 
## can cache its inverse and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
        ## @x: a square invertible matrix
        ## return: a list containing functions to
        ##      1. set the value of the matrix
        ##      2. get the value of the matrix
        ##      3. set the value of the inverse
        ##      4. get the value of the inverse
        ## this list is used as the input to cacheSolve()
        
        # initialize to NULL
        inv = NULL
        
        # create the matrix in the working environment
        ## <<- operator can be used to assign a value to an object 
        ## in an environment that is different from the current environment
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # get the value of the matrix
        get = function() x
        
        # invert the matrix and store in cache
        setinv = function(inverse) inv <<- inverse 
        
        # get the inverted matrix from cache
        getinv = function() inv
        
        # return the created functions to the working environment
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

## Computing the inverse of a square matrix can be done with the solve function.
## casheSolve computes the inverse of the “matrix” created with the makeCacheMatrix function. 
## It first checks if the inverse has already been calculated and the matrix has not changed,
## if so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse and sets the value via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        ## get the inverse of the matrix stored in cache
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                message("getting cached data")
                
                ## display matrix
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # setinv function sets the value of the inverse in the cache.
        x$setinv(inv)
        
        ## display matrix
        return(inv)
}

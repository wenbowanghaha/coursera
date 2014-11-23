## Put comments here that give an overall description of what your
## functions do

## The following functions create "special" matrix and cache the 
## inverse of the matrix. 

## Write a short comment describing this function

## makeCacheMatrix is a function that takes a matrix as the argument
## and creates a list of functions, which can get the value of the matrix,
## set a new matrix, get the inverse of the matrix and set the inverse of
## the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## Write a short comment describing this function

## cacheSolve is a function that gets the inverse of a matrix
## if the inverse has already been calculated and the matrix
## is not changed, or otherwise calculates the inverse and 
## cache it for the later use. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("Getting cache inverse...")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## Created by Cristian Monsalve 
## 23.04.2015

## The function MakeCacheMatrix is a function that creates and special
## matrix. With matrix object it is possible to store a matrix, and its
## inverse

## Asumption: the matrix storage is invertible

## This object cointains four different methods:
## set: set the matrix values
## get: get the matrix values
## setinv: sett the inverse matrix from the original
## getinv: get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Assing a NULL value to the matrix inverse
        inver <- NULL
        
        set <- function(y){
                x <<- y
                inver <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inverse) inver<<- inverse
        
        getinv <- function() inver
        
        list(set = set, get = get, setinv = setinv, getinv = getinv )
}


## cacheSolve is a function that evaluates if it has been calculated the inverse 
## for an object of makeCacheMatrix, if not it calculate it and assign 
## the inverse matrix to the matrix object using the method setinv 
# from the original object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinv()
        
        if(!is.null(inver)){
                message("getting cached data")
                return(inver)
        }
        
        matrix <- x$get()
        # Assuming that the matrix is invertible
        inver <- chol2inv(chol(matrix))
        x$setinv (inver)
}

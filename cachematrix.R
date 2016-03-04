## name: cachematrix.R
## developer: john zekind (github.com/riajz)
## date: 2016-03-04
## desc: R Programming Week 3 ( Assignment 2 )
## 
## 
## makeCacheMatrix - creates a cache of a provided matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ## setting environment data outside of function for caching operations
  
    inv <- NULL
    
    set <- function(y){
      x <<- y
      inv <<- NULL
    }

    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    ## returns list of functions for the matrix x:
    ## 1. setting a matrix
    ## 2. getting a matrix
    ## 3. setting the inverse of the matrix
    ## 4. getting the inverse of the matrix
    ## --this is used for cacheSolve()
    
    list(set=set,
         get=get,
         setinv=setinv,
         getinv=getinv)
}


## cacheSolve - returns the inverse of a matrix, first by cache if exists,
##   then by solving it directly

cacheSolve <- function(x, ...) {
  
  ## getting matrix from makeCacheMatrix
  inv <- x$getinv()
  
  ## retrieve and return cached solution if available
  if (!is.null(inv)){
    message("Retrieving cached solution...")
    return(inv)
  }
  else {
    ## otherwise calc solution
    message("Calculating solution...")
    mat.data <- x$get()
    inv <- solve(mat.data, ...)
    
    ## storing solution in cache for future retrieval
    x$setinv(inv)
    
    ##return solution
    return(inv)
  }
  
}

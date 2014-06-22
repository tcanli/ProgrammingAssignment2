## Put comments here that give an overall description of what your
## functions do

## Creates special matrix object that can cache the result of 
## its inverse operation

makeCacheMatrix <- function(x = matrix()) {
    invM <-NULL
    set <-function(y){
      x <<- y
      invM <<- NULL
    }
    get <-function() x
    setInv <-function(inv_m) invM <<-inv_m
    getInv <-function()invM
    list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## Calculates and caches inverse of cache matrix object
## The inverse of a the matrix is only calculated once
## subsequent calls returns the cached results

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im=x$getInv()
    if( !is.null(im)){
        message("getting cached inverse")
        return (im)
    }
    mtrx=x$get()
    iv=solve(mtrx)
    x$setInv(iv)
    iv
}

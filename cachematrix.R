## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates an especial object that creates a matrix and stores its inverse.


makeCacheMatrix <- function(data=numeric(),r=numeric(),c=numeric())
{
    i<-NULL
    set <- function (d,x,y)
    {
        data <<- d
        r <<- x
        c <<- y
        i <<- NULL
    }
    mtx <- matrix(data,r,c)
    
    get <- function() mtx
    setinverse <- function(inverse) i<<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}




## cacheSolve recieves an object created by the function above (makeCacheMatrix)
## and performs one of the following actions:
## 1) returns the cached inverse of the matrix (if the inverse has been already calculated and cached)
## 2) stores in the object the inverse of the matrix. The inverse can be gotten later by using the $getinverse method.
cacheSolve <- function(data,...)
{
    i <- data$getinverse()
    if(!is.null(i))
    {
        message(" Reading cached data...")
        return(i)
    }
    mtx <- data$get()
    i <- solve(mtx,...)
    data$setinverse(i)
    message("Inverse cached in object ")
}

## This set of functions calculates the inverse of a matrix and stores that inverse in makeCacheMatrix environment
## If the function is called again, it will return the stored invese instead of calculating again.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())
{
    #Set z to null value
    z <- NULL
    
    #I think this forces x and z to be part of makeCacheMatrix's environment?
    #Yes, this doesn't need to run because calling this function in the other function...
    #makes x and z part of this function's environment
    set <- function(y)
    {
        x <<- y
        z <<- NULL
    }
    
    #Function to pull data
    getdata <- function() 
    {
        x
    }
    
    #Function to set the inverse in cache
    setinverse <- function(y)
    {    
        z <<- y
    }
    
    #Function to get inverse
    getinverse <- function()
    {
        z
    }
    
    #Return list of functions
    list(setinverse=setinverse, getinverse=getinverse, getdata=getdata, set=set)
}


## This function checks if there is a cached inverse for a given dataset, and if not, calculates it

cacheSolve <- function(x, ...)
{
    #Pull inverse from other function into new variable
    z <- x$getinverse()
    
    #if stored inverse is not null, then return z
    if(!is.null(z))
    {
        message("getting cached inverse")
        return(z)
    }
    #pull data from the other function
    data <- x$getdata()
    
    #calculate inverse of data
    z <- solve(data)
    
    #cache inverse
    x$setinverse(z)
    
    #return inverse
    z
}

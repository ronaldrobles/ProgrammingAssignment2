## Put comments here that give an overall description of what your 
## functions do 
## Write a short comment describing this function 



#function similar to makeVector

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        #create  a vector of functions "set", "get", "setinv" and "getinv"        
        set <- function(y) {
                x <<- y 
                m <<- NULL 
        } 
        get <- function() x 
        setinv <- function(solve) m <<- solve 
        getinv <- function() m 
#special object created, using solve function, list functions
        list(set = set, get = get, setinv = setinv, 
             getinv = getinv) 
} 


#function similar to cachemean

cacheSolve <- function(x, ...) {

        m <- x$getinv()              
        ##  if m is not null, Return value read from Cache 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if "m" is NULL, get original Matrix and calculate inverse
        mydata <- x$get() 
        m <- solve(mydata, ...) 
        x$setinv(m) 
        ## Returm inverse 
        m 
}
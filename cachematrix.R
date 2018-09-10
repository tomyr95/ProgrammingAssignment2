## The pair of functions 'makeCacheMatrix' and 'cacheSolve' work in conjuction
## to both produce the inverse solution of an invertible matrix and if the value
## has already been calculated provide that cached value w/o calculation.

## makeCacheMatrix creates special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){  #initiate function argument x as matrix
    x <- as.data.frame(x)                   #change to atomic data as dataframe
    s <- NULL                               #initiate s (solution)
    set <- function(y) {                    #assignments in parent environment
        x <<- y
        s <<- NULL
    }
    get <- function() x                     #define getter for x
    setsoln <- function(solve) s <<- solve  #define getter for solve
    getsoln <- function() s                 #define setter for solution s
    list(set = set, get = get,              #define list for $ operator usage
         setsoln = setsoln,
         getsoln = getsoln)
}


## cacheSolve computes or returnes cached cvalue of the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {            #inititate function, additional args
    s <- x$getsoln()                        #attempt to retreave existing soln
    if(!is.null(s)) {                       #used cached data is soln exists
        message("getting cached data")
        return(s)
    }
    data <- x$get()                         #get input object, calculate soln
    s <- solve(data, ...)
    x$setsoln(s)
    sm <- as.matrix(s)                      #provide soln in matrix format
    rownames(sm) <- NULL                    #remove indexes for look
    sm                                      #return soln matrix
}

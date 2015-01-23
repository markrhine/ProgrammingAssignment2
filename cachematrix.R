## Overall: These two functions will return the inverse matrix of an input matrix.
##They will create a cache for each input, so that if user tries to calculate the inverse of
##a matrix multiple times, it will retrieve the answer from the cache instead of recalculating.

##makeCacheMatrix actually contains the cache, variable m. 
##m stores th inverse matrix if previously calculated, if not, it is null.
##Input: It takes a matrix, x, as an input argument.
##Returns: a list of four functions. Does not return x or m, but holds them.
##Note: only call it ONCE per x vector. Calling it again will reset m = NULL. Reset cache.


makeCacheMatrix <- function(x = matrix()) {
    #inverse is set to NULL. Not been calulated yet.
    inverse <- NULL
    
    #a function that will rest x matrix to whatever matri you pass in as y.
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    #a function that will return x, your input matrix.
    get <- function() x
    
    #a function that will take the calculated inverse (inv) and store in cache variable: inverse
    setInverse <- function(inv) inverse <<- inv
    
    #a function that will return the value in variable: inverse. Will eiher return the real answer or NULL
    getInverse <- function() inverse
    
    #returns all four functions, all in one list. End result of calling makeCacheMatrix
    list(nameSet = set, nameGet = get, nameSetInverse = setInverse, nameGetInverse = getInverse)
}


## CacheSolve returns the inverse of a matrix object.
## Must put result of makeCacheMatrix in as the input, NOT the matrix x itself. 'x' is deceitful.
## Will look to see if inverse has been calculated before by checking value in m.
## if m is NULL, CacheSolve will calculate the inverse of the matrix x. 
## Also it will cache the answer (inverse) if first time it calclated.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    #calls nameGetInverse to get the value of: inverse. 
    inverse <- x$nameGetInverse()
    
    #if inverse returned a value, then it has been calculated before, no need to redo it.
    #just returns the value of inverse, which is the inverse matrix
    if(!is.null(inverse)) {
        message("already been done before, getting cached answer")
        return(inverse)
    }
    #else, if inverse was NULL, it's never been calculated, so nw it will be.
    #retrieves the matrix x from makeCacheMatrix
    data <- x$nameGet()    
    
    #solve will gives us the inverse, and storeit in ans.
    ans <- solve(data)
    
    #sets the cache, "inverse", so that next time it is caleled, will have the inverse aleady.
    x$nameSetInverse(ans)
    
    #returns the inverse of matrix x
    ans
}


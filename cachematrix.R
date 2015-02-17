## Because calculating the inverse of a matrix can be computationally expensive, we want to create a cache of 
## matrices and their inverses.
## 

#initialize a Global Cache, which is a list of outputs from makeCacheMatrix
Global_Cache = c()

## Create a matrix object that caches the inverse of the matrix x if it does not already exist
## input:  a matrix X
## Output:  a list of 4 functions
##              set(y): set the value of the matrix to y
##              get(): get the value of the matrix
##              setInverse(y): set the value of the matrix inverse to y
##              getInverse(): get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        
        #Function to determine if 2 matrices are equal
        matequal <- function(x, y)
                is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
        
        # first determine if X is already cached
        
        for (i in Global_Cache) {
                if (matequal(x, i$get())) {
                        message("getting cached matrix")
                        return(i)
                }
        }
        
        #matrix X has not already been cached - create an entry in the Global_Cache
        mInverse <- NULL
        
        ## set sets the matrix and sets the inverse to NULL
        set <- function(y) {
                x <<- y
                mInverse <<- NULL
        }
        
        ## get returns the original matrix
        get <- function() x
        
        ## setInverse stores the inverse of the matrix in the cache
        setInverse <- function(inverse) mInverse <<- inverse
        
        ## getInverse returns the inverse ofthe matrix
        getInverse <- function() mInverse
        
        ## return a list of 4 functions for the matrix after adding it to the Cache
        return_list <- list(set = set, get = get,
                            setInverse = setInverse,
                            getInverse = getInverse)
        
        #update the Global Cache
        Global_Cache<<- append(Global_Cache, list(return_list))
        return_list
}


## Returns the inverse of matrix the matrix stored in x.  
## If the inverse is in the cache, return the cached value, 
##                        otherwise compute it de novo
## Input: x is a list of 4 functions from makeCacheMatrix
## Output: the inverse of the matrix stored in X

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of matrix stored in 'x'
        mInverse <- x$getInverse()
        ## first check to see if the inverse is cached
        if(!is.null(mInverse)) {
                message("getting cached inverse matrix")
                return(mInverse)
        }
        ## Inverse was not cached, so compute the inverse using "solve"
        data <- x$get()
        mInverse <- solve(data, ...) #%*% data
        
        ## store the inverse back in the cache
        x$setInverse(mInverse)
        mInverse
}

## These functions will create a special matrix object that will store it's inverse 
## in the cache. The functions will retrieve the inverse matrix from the cache if
## it has already been calculated and in the input matrix is same as before 

## This function sets and gets the matrix object and it's inverse from/to the cache
## Function takes a matrix object as input

makeCacheMatrix <- function(m = matrix()) {  
        ## initializes the matrix inverse in the curr env
        m_inv <- NULL           
        
        ## sets the matrix and inverse in the parent env
        set <- function(y) {    
                m <<- y
                m_Inv <<- NULL
        }
        
        ## returns the matrix object from the current env 
        get <- function() m
        
        ## sets the matrix inverse object in the parent env
        setInv <- function(y) m_inv <<- y
        
        ## sets the matrix inverse object in the parent env
        getInv <- function() m_inv
        
        ## returns the list of functions defined above 
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## This function takes a makeCacheMatrix object (defined above) and caches the 
## inverse checks if it already does not exist in the cache, otherwise returns 
## the cached inverse matrix

cacheSolve <- function(x, ...) {
        ## get the current cached matrix inverse object
        inv_m <- x$getInv()
        
        ## get the current cached matrix object 
        m <- x$get()
        
        ## check if there is a current cached inverse matrix object and 
        ## the passed matrix object is identical to the cached matrix object
        ## If so then return the cached inverse matrix object 
        
        if (!is.null(inv_m) && identical(x,m)){
                return(inv_m)
        }
        
        ## calculate the inverse of the matrix using the solve() function and
        ## store it in the cache 
        inv_m <- solve(m, ...)
        x$setInv(inv_m)
        inv_m
}

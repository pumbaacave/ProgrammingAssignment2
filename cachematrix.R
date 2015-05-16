##makeCacheMatrix create an object linked to a cacheable
##matrix,cacheSolve return the inversive matrix of one
##cache in the object(with cache check)

## makeCacheMatrix: This function creates a special 
##"matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setinv<-function(inverse) inv<<-inverse
    getinv<-function() inv
    list(set = set, get =get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve: This function computes the inverse of 
##the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
        if(!is.null(inv)){
            message("getting inversed matrix")
            return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setinv(inv)
        inv
        
}

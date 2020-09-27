

makeCacheMatrix <- function(x = matrix()) {
              inv<- NULL       #cached inverse of matrix
              set<-function(y){
                x<<- y
                inv<<- NULL
                
              }
              get<-function() {x}    
              setinverse<- function(inverse) {inv<<- inverse}
              getinverse<- function() {inv}
              ## returns list of functions for matrix
              list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

# The inverse of the matrix
cacheSolve <- function(x, ...) {
        inv<- x$getinverse()
        # return  cached matrix inverse if its been already computed
        if(!is.null(inv)){        #checking whether inverse is null
            message("getting cached data")
          return(inv)
        
        }
        # compute inverse of matrix
        mat<- x$get()
        inv<- solve(mat, ...)      #calculates inv value
        # cache inverse
        x$setinverse(inv)
        inv           ##returns a matrix that is inverse of 'x'
          
}

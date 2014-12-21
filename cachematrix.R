## This function creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
       m<-NULL
       set<-function(y){
             x<<-y
             m<<-NULL
         }
       get<-function() x
       setmatrix<-function(solve) m<<- solve
       getmatrix<-function() m
       list(set=set, get=get,
                      setmatrix=setmatrix,
                      getmatrix=getmatrix)  
}

## The following function calculates the inverse matrix of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and sets the value of the inverse matrix in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       m<-x$getmatrix()
       if(!is.null(m)){
             message("getting cached data")
             return(m)
         }
       matrix<-x$get()
       m<-solve(matrix, ...)
       x$setmatrix(m)
       m
}

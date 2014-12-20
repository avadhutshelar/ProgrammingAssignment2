## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function(){x}
        
        setinverse<-function(me){inv<<-me}
        
        getinverse<-function(){inv}
                
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)){
                message("getting a cached copy")
                return(inv)     #returning cached copy
        }
        data<-x$get()           #getting original matrix
        inv<-solve(data)        #calculate inverse first time
        x$setinverse(inv)       #assign calculated inverse to makeCacheMatrix
        inv                     #returning an inverted matrix
}

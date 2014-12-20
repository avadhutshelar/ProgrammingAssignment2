## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL                       #inv variable to store inverse of matrix
        
        set<-function(y){               #set Function used to reset chache matrix with new values
                x<<-y                   #<< assign a value to an object in an environment different from the current
                inv<<-NULL
        }
        
        get<-function(){x}              #function to get the current values of matrix
        
        setinverse<-function(me){inv<<-me}      # function to set the newly calculated inverse
        
        getinverse<-function(){inv}             # function to get the present inverse value
                
        list(set=set,get=get,              
             setinverse=setinverse,
             getinverse=getinverse)             #list is returned
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

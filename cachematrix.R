makeCachematrix <- function( x= matrix ()) {
        inv <- NULL
        set <- function(X) {
                x <<- X
                inv <<- NULL
        }
        get <- function(){x}
        setinverse <- function(inverse){inv <<- inverse}
        getinverse <- function(){inv}
        list (set=set, get=get,setinverse=getinverse)
}


cachesolve <- function(x,...){
        inv <- x$getinverse()
        if(!is.null(inv)){
                message('getting cached data')
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}

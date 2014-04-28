## The functions return inverse of Matrix if it is in the cache, ## otherwise the inverse is computed and then returned


## makeCacheMatrix stores the value of inverse in Cache

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setInverse<-function(inverted) m<<-inverted
getInverse<-function() m
list (set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## cacheSolve computes the inverse if it is not available in ## cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  
  if(!is.null(m)){message("getting cached data")
  return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setInverse(m)
  m
}

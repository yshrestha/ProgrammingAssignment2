## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix fuction creates a list of function to:
# 1. set function to set the matrix
# 2. get function to get the matrix
# 3. setinverse function to set the inverse of matrix
# 4. getinverse function to get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL #set function takes the argument 'y' which is matrix
  set<-function(y){
  x<<-y #assigning the input ('y') argument to 'x'    
  inv<<-NULL #assigning the NULL value to inv so that the prior execution cache is cleared from cacheSolve()  
  }
  get<-function() x # get the value of x from parent environment  
  setinverse<-function(inverse) inv<<-inverse #assign input argument to inv before the inv in parent environment
  getinverse<-function() inv
  list(
    set=set, # gives the name 'set' to the set() function
    get=get, # gives the name 'get' to the get() function
    setinverse=setinverse, # gives the name 'setinverse' to the setinverse() function
    getinverse=getinverse # gives the name 'getinverse' to the getinverse() function
  )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv<-x$getinverse() #retreive inverese of the object passed as argument
  # Now checking inv to see if the it is null as makeCacheMatrix() sets the cached inverse to NULL whenever a new vector is set into the object
  if(!is.null(inv)){ 
    message("gettting cached value")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
}

###Sample excecution
##a<-matrix(c(1:4),2,2)
##t<-makeCacheMatrix(a)
##cacheSolve(t)
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##cacheSolve(t)
##gettting cached value
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

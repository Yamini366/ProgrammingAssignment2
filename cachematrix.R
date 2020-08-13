## makeCacheMatrix creates a matrix containing following functions
## 1. set the matrix
## 2. get the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL    #initializing the inv as NULL
set<-function(y){      #function to set the matrix
           x<<-y
           inv<<-NULL
      }
      get<-function(){x}  #function to get the matrix
      setInverse <- function(Inverse)(inv<<- Inverse)  #function in setting the inverse 
      getInverse <- function()(inv)  #function in getting the inverse 
      list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


# The 'cacheSolve' function calculates the inverse of the special "matrix"
# but first checks to see if the inverse has already been calculated from the makeCacheMatrix
# If so, it get the inverse from the cache and skips the computation and displays the message.
# Otherwise, it calculates the inverse of the data and sets the value of
# the inverse in the cache via the setinverse function.



## Calculates the matrix inverse and cache it 
cacheSolve <- function(x, ...) {
       inv<- x$getInverse() ##gets the inverse
        if(!is.null(inv)) {  ##checks the condition
                message("getting cached data")
                return(inv)  #returns the value of inv
        }
        data <- x$get()   #getting the value and assigning it to data
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv  ## Return a matrix that is the inverse of 'x'
}

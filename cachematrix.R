### Coursera R programming class Programming assignment 2 
makeVector <- function( x = numeric()){
      m <- NULL
      set <- function(y){
            x<<- y
            m <<- NULL
      }
      
      get <- function() x 
      
      setmean <- function(mean){
            m <<- mean 
      }
      getmean <- function () m
      
      list(set = set, get = get,
           setmean = setmean,
           getmean = getmean)
}

cachemean <- function(x,…){
      m <- x$getmean()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
            
      }
      
      data <- x$get()
      m <- mean(data,…)
      x$setmean(m)
      m
      
}

## This function create a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()){
      inv <- NULL
      set <- function(y){
            x <<- y 
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse){
            inv <<- inverse
      }
      getinverse <- function() inv 
      list(set= set, get = get, 
           setinverse= setinverse, 
           getinverse = getinverse )
            
      
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatix above if the inverse has already been calculated
##(and the matrix has not changed), then the cache solve should retrieve the inverse from the cache

cacheSolve <- function(x,…){
      inv <- x$getinverse()
      if(!is.null(inv)){
            message("getting cached data.")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv
}

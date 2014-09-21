## The 2 functions below will allow user to cache the inverse of a matrix. This might help to save computing cost, especially when matrix inversion is usually a costly computation.

########### makeCacheMatrix ###########
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) ## ensuring the input is a matrix
{
  m<-NULL
  
  set<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  
  get<-function() x
  
  setinverse<-function(solve) m <<- solve
  
  getinverse<-function() m
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## return a list of functions
}

########### cacheSolve ###########
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  m<-x$getinverse()
  if(!is.null(m)) ## return the cached data if inverse has already been calculated
  {
    message("getting cached data")
    return(m)
  }
  else          ## calculate and return inverse of 'x' if it has not been done yet
  {
    matrix<-x$get() 
    m<-solve(matrix, ...)  ## using solve() function to calculate
    x$setinverse(m) ##caching the data
    m ## Return a matrix that is the inverse of 'x'
  }
}

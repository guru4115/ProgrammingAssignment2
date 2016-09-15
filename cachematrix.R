## The purpose of these functions are to creates a matrix 'x'
## and calculate the inverse of the matrix. The computed value(matrix inverse) 
## is placed in cache. when the function is ran again cache is checked.
## data from cache 'm'is return if available
#.................................................................................#
# The Function makeCacheMatrix(x) creates a matrix 'x' and a cache 'm'
# m is initialize with NA and reseted NA if a new x is inserted
# and the cache NA when a new x is inserted
makeCacheMatrix <- function(x = matrix()) {
  m <- matrix() # x is initialize with NA 
  set <- function(y) {
    x <<- y
    m <<- matrix() # reset cache with NA   
  }
  get <- function() x # return  x
  setmatrix <- function(matrix) m <<- matrix #set the cache
  getmatrix <- function() m # return cached data
  list(set = set, get = get, # enable the use of $ operator
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}



# This function calculate the inverse of the matrix x 
# returned by makeCacheMatrix above.  
# If the inverse is available in cache the data is return from cache

cacheSolve <- function(x, ...) 
{
  m <- x$getmatrix()
  if(all(!is.na (m)))# check if data is in cache
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) # inverse x
  x$setmatrix(m) # set cache
  m      
}

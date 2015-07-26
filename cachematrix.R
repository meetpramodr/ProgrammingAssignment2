## makeCache stores and retrieves the matrices from the cache
## cacheSolve computes the inverse if the inverse in not in cache, else returns the inv matrix from cache

## makeCacheMatrix has 4 methods - set, get, getCache, setCache. 
## set function assigns values of the matrix and get returns the matrix
## getCache returns the matrix stored in the cache and setCache stores the matrix is cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x  
  getCache <- function() m
  setCache <- function(inv) m<<-inv
  list(set=set, get=get, getCache=getCache, setCache=setCache)
}


## cacheSolve first attempts to get the matrix inverse from cache. 
## If it is found (not null), then returns the inverse matrix
## If it is not found, then it computes the inverse and pushes it into cache (using setCache) and returns the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getCache()
  if (!is.null(m)){
    print ("From Cache")
    return (m)
  }
  
  inv <- solve(x$get())
  x$setCache(inv)
  inv
}

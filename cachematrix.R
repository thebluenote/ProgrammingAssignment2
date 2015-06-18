## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to: 1.- set the value of the matrix; 2.- get the value of the matrix; 3.- set the value of the inverse matrix; 4.- get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL                            
  set <- function(y) {  
    x <<- y
    m <<- NULL      
  }
  get <- function() x  
  setinverse <- function(inverse) m <<- inverse  
  getinverse <- function() m  
  list(set = set, get = get,  
       setinverse = setinverse, 
       getinverse = getinverse) 
}

## The function cacheSolve checks if the inverse has already been calculated. Is so, it returns the cached result. Otherwise, it calculates it and caches it via makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()   
  if(!is.null(m)) {  
    message("getting cached data")
    return(m)
  }                   
  data <- x$get()     
  m <- solve(data, ...)
  x$setinverse(m)        
  m          
}
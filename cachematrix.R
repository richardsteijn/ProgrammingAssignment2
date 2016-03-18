##Assignment: Cashing the inverse of a matrix
 

##This function receives a "matrix" object as its argument that can caches it 
##It is meant to work in conjunction with the function cacheSolve 
##The return value of this function can be used to 
##retrieve (get) the cashed matrix 
##replace (set) the cashed matrix with a new one
##store (setsol) de result of de solve(x) function
##retrieve (getsol) de result of de solve(x) function

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  
  setsol <- function(sol) s <<- sol
  
  getsol <- function() s
  
  list(set = set, get = get,
       setsol = setsol,
       getsol = getsol)  
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve retrieves the inverse from the cache. 
 
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  s <- x$getsol()
  
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  data <- x$get()
  s <- solve(data, ...)
  x$setsol(s)
  
  s
}

 
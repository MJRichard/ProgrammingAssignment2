
## makeCacheMatrix stores a matrix object and also give it the properties
## assigned to it which include set, get, setinv, and getinv.
## cacheSolve accesses this function to solve the matrix


makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y) {
      x <<- y
      mat <<- NULL
    }
    get <- function() x
    setinv <- function(solve) mat <<- solve #solve for the inverse when it is called
    getinv <- function() mat 
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv) #listing properties of the makeCacheMatirx object

}


## cacheSolve uses the propeties of makeCacheMatrix to either solve for the inverse
## of a Matrix or access the value of the inverse if it has already been solved for.
## It notifies the user if the inverse has already been cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv() #tries to find the cached inverse value
  if(!is.null(inv)) { #already cached the matrix
    message("getting cached data")
    return(inv) #returning the cached inverse value
  }
  #otherwise calculate the inverse
  data <- x$get()
  mat <- solve(data, ...) #caching the inverse
  x$setinv(mat)
  mat
}

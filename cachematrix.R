##There are two functions below to calculate the inverse of a matrix.  Since the calculation of the inverse
##of a matrix is a time-consuming task, certain variables are used to determine if the matrix has changed.
##If the matrix has not changed, the result previously calcualted is recalled from the global cache (using <<-).
##If the matrix has changed, the inverse matrix is calculated and stored in the global cache.

##First function (makeCacheMatrix) extracts the values of the matrix, and calcualtes the inverse matrix.
##the inv parameter is set to NULL so that it can be used later to determine if the inverse has already been
##calculated.  The input is a matrix, and the output is a list of four parameters: $set receives the input matrix,
##$get outputs the input matrix, $setInverse receives the inverse matrix, and $getInverse outputs the inverse matrix.
##The order in which the commands should be carried out are: set, get, setInverse and getInverse.
##makeCacheMatrix needs to be <- to a variable so the result is stored in it.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL    ##parameter to determine later if inverse has been calculated or not.
  set <- function(y) {  ##set will fill the matrix for which the inverse is to be calculated    
    x <<- y
    inv <<- NULL        ##double-verify that the inverse has not been calcualated, even when a new matrix is set
  }
  get <- function() x   ##show matrix that is to be calculated the inverse for
  setInverse <- function() inv <<- solve(x) ##calculate the inverse matrix
  getInverse <- function() inv  ##output the inverse matrix
  list(set = set,               ##this is the output list with all the four parameters as defined above.
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##Second function (cacheSolve) receives the list output from makeCacheMatrix and verifies if the matrix is new, or
##if the inverse has already been calculated by verifying if $getInverse is NULL.  If $getInverse is not NULL, 
##inverse has already been calculated and results from global cache are presented.  In $getInverse is NULL, then
##the matrix presented is new, and a new inverse needs to be calculated.  The general intent is for the first function
##makeCacheMatrix to be called only once at the beginning of the program, and cacheSolve several times every time the
##inverse of the matrix is necessary.  When a new matrix is used, $set needs to be equated to the new matrix, and the
##$setInverse is equated to NULL, thus, forcing a new inverse matrix calculation.

cacheSolve <- function(x=list,...) {
  m<-x$getInverse()    ##determine if inverse has alredy been calculated (not NULL) or not (NULL)
  if(!is.null(m)){
    message("getting cached data")  ##inverse has already been calculated, so extract output from global cache,
    return(m)                       ##and output to console.
  }else{
    matrix<-x$get()           ##if matrix is new, new inverse needs to be calcualated, and all four parameters
    x$set(matrix)             ##of the list need to be filled.
    m<-solve(matrix, ...)
    x$setInverse()
    x$getInverse()
  }
}
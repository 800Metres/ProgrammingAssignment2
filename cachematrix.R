## This R file contains the primary functions makeCacheMatrix() and cacheSolve, plus some helper functions to generate sample squared invertble matrices that can be used if needed. Calling showSamples() displays 5 options

## makeCacheMatrix returns a special matrix that is able to cache it's inverse the function expects a square invertible matrix and performs some simple validation checks
makeCacheMatrix <- function(x = matrix()) {
  #basic validation moved outside of the core function to keep the code readable
  validateMatrix(x)
  inv <- NULL
  mtx <- x
  #simple getters and setters around the matrix and inverse, which if used invalidate any cached values
  setMatrix <- function(m){
    validateMatrix(m) 
    print("setting matrix & clearing cached inverse")
    mtx <<- m
    inv <<- NULL
  }
  getMatrix <- function() mtx
  setInverse <- function(i) inv <<- i
  getInverse <- function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve returns the inverse of special matrix object, either retrieving the cached value 
## or generating the inverse and then caching it for later retrieval
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)){
    message("Inverse is already cached...")
    return(i)
  }
  
  #if not cached, then retrieve the matrix, generate the inverse, cache & return
  m <- x$getMatrix()
  i <- solve(m)
  x$setInverse(i)
  i
}

## Simple function to validate the supplied matrix and gracefully degrade on error
validateMatrix <- function(mtx){
  errorText = "Parameter is not a square invertible matrix"
  #stop programme if supplied parameter is not a matrix
  if(!is.matrix(mtx)) 
    stop(errorText)
  #stop if mtx is not empty & it's either not square or not invertible
  if(length(mtx)>1){
    if(det(mtx)==0 || dim(mtx)[1] != dim(mtx)[2])
      stop(errorText)
  }
}

## Returns one of the sample matrices x is the number of the Matrix as displayed in the showSamples() function
## An example of how this is used would be:
##  showSamples() # lists 5 matrices
##  testMatrix <- getSampleMatrix(3) #the user has chosen matrix 3 from the above
##  cacheMatrix <- makeCacheMatrix(testMatrix)
##  cacheSolve(cacheMatrix) #inverse of matrix 3 won't be cached yet so will generate lazily
##  cacheSolve(cacheMatrix) #inverse of matrix 3 was generated in the above so cached value will be used
getSampleMatrix <- function(x){
  if(x<0 || x>5){
    stop("Only 5 sample matrixes were generated. Please call this function passing in 1 to 5")
  }
  message(paste("Matrix Chosen is:", x))
  generateSampleMatrixes()[[x]]
}

## showSamples prints 5 sample matrices
showSamples <- function(){
  samples <- generateSampleMatrices();
  message("Choose a matrix:")
  for (i in 1:length(samples)){
    message(paste("Matrix",i,":"))
    print(samples[[i]])
  }
  message("If you'd like to use any of these matrices, call getSampleMatrix(x) with the matrix number to return a matrix that can then be passed into createCacheMatrix()")
}


## generateSampleMatrixcs (repeatedly) generates 5 (5*5) sample square invertible matrices containing 25 numbers from 1:1000 to help test the above primary functions
generateSampleMatrices <- function(){
  sample = list()
  set.seed(1)
  for(i in 1:5){
    s <- sample(1:1000,25,replace=T)
    mtx <- matrix(s,5,5)
    if(det(mtx)!=0){
      sample[[i]] = mtx
    }
  } 
  sample
}
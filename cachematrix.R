## Arun Natesan - Programming Assignment #2 
## Version 1.4 
## Submitted: March-22-2015 

## Function #1 - makeCacheMatrix() 

## Purpose: Create Special "matrix" object, with 4 functions described below. 
##          The purpose is to be able to create, return and save and return saved matrix 

## Output: List containing 4 functions (explained below)

##  setMatrix: Function to set the value of the Square Matrix (assumed it can be invertible)
##             Ex: a <- makeCacheMatrix(matrix(c(1:4),2,2)) will create a 2X2 square matrix and invoke setMatrix to set value

##  getMatrix: Function to return the value of the Square Matrix 
##             Ex: a$getMatrix() 

##  setCacheInverse: Function to store (cahce) the inverse value of the matrix 
##             Ex: b<-solve(matrix(c(1:4),2,2)) 
##                a$setCacheInverse(b)

##  getCacheInverse: Function to return (cahced) inverse value of the matrix 
##             Ex: a$getCacheInverse() 

makeCacheMatrix <- function(x = matrix()) {       ## It is assumed the matrix is square and it can be inversed based on the assignemnt 
  
  inverse <- NULL   		                  ## Clear the local inverse  
  
  setMatrix <- function(y) {		          ## Function to Set Square Matrix
    x <<- y 				          ## Set input matrix value to x using "<<"
    inverse <<- NULL                              ## Clear the inverse value using "<<" 
  }
  
  getMatrix <- function() x 			  ## Simply return matrix x 
  
  setCacheInverse <- function(inv_matrix) {
    inverse <<- inv_matrix                        ## Set the inverse using "<<"
  }
  
  getCacheInverse <- function() inverse 	   ## Simply return local inverse  
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, ## Return a list of functions 
       setCacheInverse = setCacheInverse,
       getCacheInverse = getCacheInverse)
}

## Arun Natesan - Programming Assignment #2 
## Version 1.4 
## Submitted: March-22-2015 

## Function #2 - CacheSolve() 

## Purpose: Invert a square matrix object. 
## It shoule be created using makeCacheMatrix 
## Use solve() to return inverted matrix 
## If inverted matrix is already available (check using makeCacheMatrix), return without re-calculating

## Input: Matrix Object

## Output: Inverted Matrix Object 

## Please refer the previous function for "a". 
## Ex: cacheSolve(a) will return the inverse matrix from cache if is was inversed atleast once 

cacheSolve <- function(x, ...) {			
  
## It is assumed the special matrix x is created using makeCacheMatrix() above  

## Get inverse from getCacheInverse first. Note: It may or may not have been cached before
  
  inverse <- x$getCacheInverse()
  
## If the cache exist, result is NOT null 
  
  if(!is.null(inverse)) {					                
    message("getting cached inverse matrix")			
## Print message to prove the result is from Cache 
## Return the invers, without executing solve()
    return(inverse)
  }
  
  a <- x$getMatrix()		## Get matrix value 
  inverse <- solve(a, ...)	## Set inverse matrix
  x$setCacheInverse(inverse)	## Since this is the first time, this cache is saved, for future 
  inverse 			## Return the inverse matrix
}



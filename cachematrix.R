## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makecacheInvatrix <- function(x = matrix()) {
  cacheInv <- NULL
  set <- function(mY){
    x <<- mY
    cacheInv <<- NULL
    
  }
  get <- function() x
  setInvMatrix <- function(invMat) {
    cacheInv <<- invMat
  }
  getInvMatrix <- function() cacheInv
  list(set = set, get = get, 
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
  
}


## if Inverse of matrix is cached then retrieves it from the cache else 
##  a) inverses the matrix
## b) caches the inverse of matrix
## c) returns the inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheInv <- x$getInvMatrix()
  if(!is.null(cacheInv))   {
    message("getting cached data")
    return(cacheInv)
  }
  data <- x$get()
  cacheInv <- solve(data)
  x$setInvMatrix(cacheInv)
  cacheInv
}
# 
# Sample test
# > a1 <- c(3, 2, 5,6) 
# > a2 <- c(2, 3, 2,7) 
# > a3 <- c(5, 2, 4,8) 
# > a4 <- c(5, 2, 5,9) 
# > A <- rbind(a1,a2,a3,a4)
# > A
# [,1] [,2] [,3] [,4]
# a1    3    2    5    6
# a2    2    3    2    7
# a3    5    2    4    8
# a4    5    2    5    9
# 
# 
# 
# > x <- makecacheInvatrix(A)
# > cacheSolve(x)
# a1         a2         a3          a4
# [1,]  0.06896552 -0.2068966  1.1379310 -0.89655172
# [2,]  0.58620690  0.2413793  1.1724138 -1.62068966
# [3,]  0.37931034 -0.1379310 -0.2413793  0.06896552
# [4,] -0.37931034  0.1379310 -0.7586207  0.93103448
# > cacheSolve(x)
# getting cached data
# a1         a2         a3          a4
# [1,]  0.06896552 -0.2068966  1.1379310 -0.89655172
# [2,]  0.58620690  0.2413793  1.1724138 -1.62068966
# [3,]  0.37931034 -0.1379310 -0.2413793  0.06896552
# [4,] -0.37931034  0.1379310 -0.7586207  0.93103448
# > 




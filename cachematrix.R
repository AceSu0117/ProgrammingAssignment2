## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = numeric()){
  inv <- NULL 
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
# makeCacheMatrix 函数用于创建一个特殊的矩阵对象，该对象存储一个矩阵及其缓存的逆矩阵。

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  
  x$setInverse(inv)
  
  inv
}
# cacheSolve 函数用于计算并返回 makeCacheMatrix 创建的矩阵对象的逆矩阵，如果逆矩阵已缓存则直接返回缓存值。

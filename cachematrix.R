## The R code comprises two functions: makeCacheMatrix() and cacheSolve().
## These functions are used to generate a matrix that has the ability to cache its inverse.
## To test the code:
## 
## source(cachematrix.R)
## SAMPLE <- rbind(c(1,0,0), c(0,1,0), c(0,0,1))
## CACHED_SAMPLE = makeCacheMatrix(x)
## cacheSolve(CACHED_SAMPLE) # this is the matrix inverse


# makeCacheMatrix() generates a matrix that can cache its inverse.
makeCacheMatrix <- function(INPUT = matrix()) {

  # Initialization
  MATR_INIT <- NULL


  FUNC_SET <- function(Z) {
    INPUT <<- Z
    MATR_INIT <<- NULL
  }

  # FUNC_GET: its purpose is to get the matrix value
  # SET_INV : this sets the inverse
  # GET_INV : its function is to get the inverse
  FUNC_GET <- function() INPUT
  SET_INV <- function(INV) MATR_INIT <<- INV
  GET_INV <- function() MATR_INIT

  # Return the list containing the necessary functions
  list(FUNC_SET = FUNC_SET, FUNC_GET = FUNC_GET,
       SET_INV = SET_INV,
       GET_INV = GET_INV)        
        
}


## cacheSolve() determines the inverse of the matrix returned by function makeCacheMatrix().
## Note that if the matrix inverse has already been calculated,
## cacheSolve() spews out the cached inverse.

cacheSolve <- function(INPUT, ...) {
  INVERSE_MATRIX <- INPUT$GET_INV()
  if(!is.null(INVERSE_MATRIX)) {
    message("Inverse already calculated. Retrieving cached result...")
    return(INVERSE_MATRIX)
  }

  # If matrix inverse hasn't been solved, compute for it.
  # The inverse of a matrix is calculated using solve() function.
  MATRIX_VAL <- INPUT$FUNC_GET()
  INVERSE_MATRIX <- solve(MATRIX_VAL)

  # Cache the inverse
  INPUT$SET_INV( INVERSE_MATRIX )

  # Return the matrix inverse
  INVERSE_MATRIX
}

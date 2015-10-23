## makeCacheMAtrix and cacheSolve are used to cache the inverse
# of the matrix, so that repeat calculations are not performed

## makeCacheMatrix does the following
# 1. Set the value of the Input
# 2. Get the value of the Input
# 3. Set the value of the inverse
# 4. Get the value of the Inverse
makeCacheMatrix <- function(input = matrix()) 
{
	# set inverse to NULL
	inv <- NULL
	# set is the input
	set <- function(input.matrix) 
	{
		#input is set to input variable
		input <<- input.matrix
		# inv is still null
		inv <<- NULL
	}
	# get is set to input
	get <- function() input
	# inv is set to the inverse passed to the variable
	setinverse <- function(inverse) inv <<- inverse
	# getinverse variable is set to inv
	getinverse <- function() inv
	#list is set
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix

cacheSolve <- function(input.matrix, ...)
{
    ## Return a matrix that is the inverse of 'x'

    inv.matrix <- input.matrix$getinverse()
    if(!is.null(inv.matrix)) 
    {
    	message("Cached data has been returned.")
    	return(inv.matrix)
    }
    data <- input.matrix$get()
    inv.matrix <- solve(data)
    input.matrix$setinverse(inv.matrix)
    inv.matrix
}

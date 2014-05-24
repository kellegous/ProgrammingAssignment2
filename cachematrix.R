## Functionality for a matrix object that is capable of caching
## the results of expensive calculations such that repeated calls
## to expensive operations (like taking the inverse) are faster.

## Create a new matrix that is capable of caching results
## for the inverse.
makeCacheMatrix <- function(x = matrix()) {
	# A list that holds cached objects
	cache <- list()
	list(
		set = function(y) {
			x <<- y
			cache <<- list()
		},
		get = function() x,
		solve = function(...) {
			ans <- cache$solve
			if (is.null(ans)) {
				ans <- solve(x, ...)
				cache$solve <<- ans
			}
			ans
		})
}


## Inverts the matrix represented by x. This function will
## use an internally cached value if the inverse was previously
## computed.
cacheSolve <- function(x, ...) x$solve(...)


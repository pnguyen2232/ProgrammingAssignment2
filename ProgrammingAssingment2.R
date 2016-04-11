makeVector <- function(x = numeric()){
	m<- NULL
	set<- function(y) {
		x<<- y
		m<<- NULL
	}
	get<- function() x
	setmean <- function(mean) m<<-mean
	getmean <- function() m
	list(set = set, get = get,
		setmean = setmean,
		getmean = getmean)
}



cachemean <- function(x, ...) {
	m<- x$getmean()
	if(!is.null(m)){
		message("getting cached data"
		return(n)
	}
	data <- x$get()
	m<- mean(data, ...)
	x$setmean(m)
	m
}

##this is the functin to cache a matrix

makeCacheMatrix <- function(x=matrix()) {
	##empty the inv value, just in case
	inv=NULL

	##set the matrix
	set = function(y){

		x<<-y
		##y is set as value for the matrix 'x'

		inv<<-NULL
	}

	##get the matrix
	get = function () x
	##set the inverse, and the value for it
	setinv = function (inverse) inv <<- inverse
	##get the inverse
	getinv = function() inv
	list(set=set, get=get, 
	setinv=setinv, getinv=getinv)
}


cacheSolve <- function(x, ...) {
	##get the inverse of the matrix 'x'
	inv=x$getinv()

	##if the inverse already exist in the cache, grab that and return the following message
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	
	##otherwise, calculate the matrix
	mat.data=x$get()
	inv=solve(mat.data,...)
	
	##cache the inverse with 'setin'
	x$setinv(inv)
	return(inv)
}

## test how long it takes to run inverse without cached data and then with cached data

test1= function(mat){

	tempr = makeCacheMatrix(mat)

	start.time=Sys.time()
	cacheSolve(tempr)
	z = Sys.time() - start.time
	
	print(z)
	
	##do same thing second time, it will be cached
	start.time = Sys.time()
	cacheSolve(tempr)
	z = Sys.time()-start.time
	print(z)
}

##used the following to test the test1 function

set.seed(12345)
r=rnorm(1000000)
mat1= matrix(r, nrow=1000,ncol=1000)
test1(mat1)
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


makeCacheMatrix <- function(x=matrix()) {
	inv=NULL
	set = function(y){
		x<<-y
		inv<<-NULL
	}
	get = function () x
	setinv = function (inverse) inv <<- inverse
	getinv = function() inv
	list(set=set, get=get, 
	setinv=setinv, getinv=getinv)
}


cacheSolve <- function(x, ...) {
	inv=x$getinv()

	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	
	mat.data=x$get()
	inv=solve(mat.data,...)
	
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